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

fn init_market() -> Result(Market, Nil) {
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
  use max_buying <- result.map(max(m.buying))

  Market(
    ..m,
    selling: replace_x(m.selling, ask_price, spread() + max_buying),
    min_history: set_nth_element(m.min_history, time_idx, ask_price),
    delta: ask_price - m.last_price,
  )
}

fn sell(m: Market, time_idx) -> Result(Market, Nil) {
  // Get the best price to sell to
  use bid_price <- result.try(list.reduce(m.buying, int.max))
  use min_selling <- result.map(list.reduce(m.selling, int.min))

  Market(
    ..m,
    // Replace used offer with a new one via artificial spread
    buying: replace_x(m.buying, bid_price, spread() + min_selling),
    // Record transaction
    max_history: set_nth_element(m.max_history, time_idx, bid_price),
    delta: bid_price - m.last_price,
  )
}

fn set_nth_element(l: List(a), i: Int, v: a) -> List(a) {
  echo #(i, v)
  let assert #(first, [_, ..rest]) = list.split(l, i)
  list.append(first, [v, ..rest])
}

fn chart(m: Market) -> Result(Nil, Nil) {
  use history <- result.try(list.strict_zip(m.min_history, m.max_history))
  assert list.length(history) == time + 1

  // Get height needed to fit chart
  use chart_height <- result.map(max(list.append(m.min_history, m.max_history)))

  let chart_text =
    history
    // Paint chart intervals
    |> list.map(fn(v) {
      let #(from, to) = v
      list.range(1, chart_height)
      |> list.map(fn(x) {
        case x {
          x if x > from && x < to -> "|"
          x if x == from || x == to -> "+"
          _ -> " "
        }
      })
    })
    // Organize data to final position in matrix
    |> list.map(list.reverse)
    |> list.transpose
    // Paint y axis
    |> list.map(fn(col) { ["|", ..col] })
    // Paint x axis
    |> list.append([
      list.range(0, time + 1) |> list.map(fn(_) { "-" }),
    ])
    // Make matrix into plain text
    |> list.map(fn(row) { string.concat(row) })
    |> string.join("\n")

  io.print("\n\n" <> chart_text)
  Nil
}

fn buy_or_sell(m: Result(Market, Nil), t) -> Result(Market, Nil) {
  // Decision
  use m <- result.try(m)
  use m1 <- result.try(case float.random() {
    x if x <. 0.5 -> buy(m, t)
    _ -> sell(m, t)
  })

  // Tendency, to make it more divergent
  use m2 <- result.try(case m1.delta <= 0 {
    True -> buy(m1, t)
    False -> sell(m1, t)
  })
  case m2.delta <= 0 {
    True -> buy(m2, t)
    False -> sell(m2, t)
  }
}

pub fn main() -> Result(Nil, Nil) {
  // Run simulation
  list.range(0, time)
  |> list.fold(init_market(), buy_or_sell)
  |> result.try(chart)
}
