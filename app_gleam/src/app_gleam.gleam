import gleam/float
import gleam/int
import gleam/io
import gleam/list
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

fn init_market() -> Market {
  let buying =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) })
  let selling =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) + 25 })
  let max_history = list.repeat(0, time + 1)
  let min_history = list.repeat(100, time + 1)
  let last_price = min(selling)

  Market(buying, selling, max_history, min_history, last_price, 0)
}

fn min(l: List(Int)) -> Int {
  let assert [first, ..rest] = l
  list.fold(rest, first, int.min)
}

fn max(l: List(Int)) -> Int {
  let assert [first, ..rest] = l
  list.fold(rest, first, int.max)
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

fn buy(m: Market, time_idx) -> Market {
  // Get the best price to buy at
  let ask_price = min(m.selling)

  Market(
    ..m,
    selling: replace_x(m.selling, ask_price, spread() + max(m.buying)),
    min_history: set_nth_element(m.min_history, time_idx, ask_price),
    delta: ask_price - m.last_price,
  )
}

fn sell(m: Market, time_idx) -> Market {
  // Get the best price to sell to
  let bid_price = max(m.buying)

  Market(
    ..m,
    // Replace used offer with a new one via artificial spread
    buying: replace_x(m.buying, bid_price, spread() + min(m.selling)),
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

fn chart(m: Market) {
  let assert Ok(history) = list.strict_zip(m.min_history, m.max_history)
  echo m.min_history
  echo m.max_history
  assert list.length(history) == time + 1

  // Get height needed to fit chart
  let chart_height = max(list.append(m.min_history, m.max_history))

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

pub fn main() -> Nil {
  let m = init_market()

  // Run simulation
  list.range(0, time)
  |> list.fold(m, fn(m, t) {
    // Decision
    let m1 = case float.random() {
      x if x <. 0.5 -> buy(m, t)
      _ -> sell(m, t)
    }
    // Tendency, to make it more divergent
    case m1.delta <= 0 {
      True -> buy(m1, t) |> buy(t)
      False -> sell(m1, t) |> sell(t)
    }
  })
  |> chart
}
