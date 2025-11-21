import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/string

// This doesn't work completely, but oh well. It could probably be simpler

const time = 10

const number_of_offers = 10

fn offer_margin() -> Int {
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
  let max_history = list.repeat(time, 0)
  let min_history = list.repeat(time, 1000)

  let last_price = min(selling)

  Market(buying, selling, max_history, min_history, last_price, 0)
}

fn min(l: List(Int)) -> Int {
  let assert [first, ..rest] = l
  list.fold(rest, first, fn(a, b) {
    case a <= b {
      True -> a
      False -> b
    }
  })
}

fn max(l: List(Int)) -> Int {
  let assert [first, ..rest] = l
  list.fold(rest, first, fn(a, b) {
    case a >= b {
      True -> a
      False -> b
    }
  })
}

pub fn remove_first_occurrence(list: List(a), item: a) -> List(a) {
  case list {
    [] -> []
    // empty list, return as is
    [first, ..rest] ->
      case first == item {
        True -> rest
        // skip this first matching item
        False -> [first, ..remove_first_occurrence(rest, item)]
        // keep first, recurse on rest
      }
  }
}

fn buy(market: Market, time_idx: Int) -> Market {
  let price = min(market.selling)
  let selling = remove_first_occurrence(market.selling, price)
  let market = exchange(market, price, time_idx)
  let selling = [offer_margin() + max(market.buying), ..selling]
  Market(..market, selling:)
}

fn sell(market: Market, time_idx: Int) -> Market {
  // Get the best price to sell to
  let price = max(market.buying)
  let buying = remove_first_occurrence(market.buying, price)
  let market = exchange(market, price, time_idx)
  // Replenish offer with a new one
  let buying = [offer_margin() + max(market.buying), ..buying]
  Market(..market, buying:)
}

fn nth_element(list: List(a), i: Int) -> a {
  assert i >= 0
  let assert [first, ..rest] = list
  case i {
    0 -> first
    _ -> nth_element(rest, i - 1)
  }
}

fn set_nth_element(list: List(a), i: Int, v: a) -> List(a) {
  assert i >= 0
  let assert [first, ..rest] = list
  case i {
    0 -> [v, ..rest]
    _ -> [first, ..set_nth_element(rest, i - 1, v)]
  }
}

fn exchange(market: Market, price: Int, time_idx: Int) -> Market {
  let delta = price - market.last_price
  let last_price = price

  let min_history =
    set_nth_element(
      market.min_history,
      time_idx,
      min([price, nth_element(market.min_history, time_idx)]),
    )

  let max_history =
    set_nth_element(
      market.min_history,
      time_idx,
      max([price, nth_element(market.min_history, time_idx)]),
    )

  Market(
    market.buying,
    market.selling,
    max_history,
    min_history,
    last_price,
    delta,
  )
}

fn chart(m: Market) {
  let history = list.zip(m.min_history, m.max_history)

  // Get height needed to fit chart
  let chart_height =
    history
    |> list.map(fn(r) {
      let #(first, sec) = r
      int.max(first, sec)
    })
    |> max

  let chart_text =
    history
    // Paint chart intervals
    |> list.map(fn(v) {
      let #(from, to) = v
      list.reverse(
        list.map(list.range(1, chart_height), fn(x) {
          case x {
            x if x > from && x < to -> "|"
            x if x == from || x == to -> "+"
            _ -> " "
          }
        }),
      )
    })
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
}

pub fn main() -> Nil {
  let m = init_market()

  // Run simulation
  list.repeat(0, time)
  |> list.fold(m, fn(m, t) {
    // Decision
    case float.random() {
      x if x <. 0.5 -> buy(m, t)
      _ -> sell(m, t)
    }
    |> fn(m1) {
      // Tendency, to make it more divergent
      case m1.delta <= 0 {
        True -> buy(m1, t) |> buy(t)
        False -> sell(m1, t) |> sell(t)
      }
    }
  })
  |> chart
}
