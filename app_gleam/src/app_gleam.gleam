import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

// This doesn't work completely, but oh well. It could probably be simpler

const time = 100

const number_of_offers = 10

fn spread() -> Int {
  int.random(10)
}

type Market {
  Market(
    buying: List(Int),
    selling: List(Int),
    max_history: List(Int),
    min_history: List(Int),
    last_price: Int,
    delta: Int,
  )
}

fn min(l: List(Int)) {
  list.reduce(l, int.min)
}

fn max(l: List(Int)) {
  list.reduce(l, int.max)
}

fn replace_x(l, old_value, new_value) {
  case l {
    [] -> []
    [first, ..rest] ->
      case first == old_value {
        True -> [new_value, ..rest]
        False -> [first, ..replace_x(rest, old_value, new_value)]
      }
  }
}

fn buy(m: Market, time_idx) -> Result(Market, Nil) {
  // Get the best price to buy at
  use ask_price <- result.try(min(m.selling))
  use min_history <- result.try(set_nth_element(
    m.min_history,
    time_idx,
    ask_price,
  ))
  use max_buying <- result.try(max(m.buying))
  let selling = replace_x(m.selling, ask_price, spread() + max_buying)

  Ok(Market(..m, selling:, min_history:, delta: ask_price - m.last_price))
}

fn sell(m: Market, time_idx) -> Result(Market, Nil) {
  // Get the best price to sell to
  use bid_price <- result.try(max(m.buying))
  // Record transaction
  use max_history <- result.try(set_nth_element(
    m.max_history,
    time_idx,
    bid_price,
  ))

  use min_selling <- result.map(min(m.selling))

  Market(
    ..m,
    // Replace used offer with a new one via artificial spread
    buying: replace_x(m.buying, bid_price, spread() + min_selling),
    max_history:,
    delta: bid_price - m.last_price,
  )
}

fn set_nth_element(l: List(a), i: Int, v: a) -> Result(List(a), Nil) {
  let #(first, second) = list.split(l, i)
  case second {
    [] -> Error(Nil)
    [_nth, ..rest] -> Ok(list.append(first, [v, ..rest]))
  }
}

fn initial_state() -> Result(Market, Nil) {
  let buying =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) })
  let selling =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) + 25 })
  let max_history = list.repeat(0, time + 1)
  let min_history = list.repeat(100, time + 1)

  use last_price <- result.try(min(selling))
  Ok(Market(buying, selling, max_history, min_history, last_price, 0))
}

fn run_simulation(m: Market) {
  let decision = fn(m, t) {
    case float.random() {
      x if x <. 0.5 -> buy(m, t)
      _ -> sell(m, t)
    }
  }
  let add_divergence = fn(m: Result(Market, Nil), t) {
    use m1 <- result.try(m)
    case m1.delta <= 0 {
      True -> buy(m1, t) |> result.try(fn(m) { buy(m, t) })
      False -> sell(m1, t) |> result.try(fn(m) { sell(m, t) })
    }
  }
  list.fold(list.range(0, time), Ok(m), fn(acc, t) {
    use m <- result.try(acc)
    decision(m, t) |> add_divergence(t)
  })
}

fn chart(m: Market) {
  use history <- result.try(list.strict_zip(m.min_history, m.max_history))
  assert list.length(history) == time + 1

  use chart_height <- result.map(max(list.append(m.min_history, m.max_history)))

  let paint_intervals = fn(v) {
    let #(from, to) = v
    list.range(1, chart_height)
    |> list.map(fn(x) {
      case x {
        x if x > from && x < to -> "|"
        x if x == from || x == to -> "+"
        _ -> " "
      }
    })
  }
  let fit_matrix = fn(v) { v |> list.map(list.reverse) |> list.transpose }
  let paint_axis = fn(v) {
    v
    |> list.map(fn(col) { ["|", ..col] })
    |> list.append([
      list.range(0, time + 1) |> list.map(fn(_) { "-" }),
    ])
  }
  let to_plain_text = fn(v) {
    v
    |> list.map(fn(row) { string.concat(row) })
    |> string.join("\n")
  }

  let chart_text =
    history
    |> list.map(paint_intervals)
    |> fit_matrix
    |> paint_axis
    |> to_plain_text

  io.print("\n\n" <> chart_text)
  Nil
}

pub fn main() -> Result(Nil, Nil) {
  initial_state() |> result.try(run_simulation) |> result.try(chart)
}
