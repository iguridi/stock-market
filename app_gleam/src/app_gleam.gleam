import gleam/int
import gleam/io
import gleam/list

import non_empty_list.{type NonEmptyList}
import non_empty_list as nel

const time = 100

const number_of_offers = 10

pub fn offer_margin() -> Int {
  int.random(10)
}

type Market {
  Market(
    buying: NonEmptyList(Int),
    selling: NonEmptyList(Int),
    max_history: NonEmptyList(Int),
    min_history: NonEmptyList(Int),
    last_price: Int,
    delta: Int,
  )
}

fn init_market() -> Market {
  let buying =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) })
    |> nel.from_list
  let selling =
    list.repeat(0, number_of_offers)
    |> list.map(fn(_) { int.random(25) + 25 })
    |> nel.from_list
  let max_history = list.repeat(time, 0)
  let min_history = list.repeat(time, 1000)

  let last_price = min(selling)

  Market(buying, selling, max_history, min_history, last_price, 0)
}

fn min(l: NonEmptyList(Int)) -> Int {
  nel.reduce(l, fn(a, b) {
    case a <= b {
      True -> a
      False -> b
    }
  })
}

fn max(l: NonEmptyList(Int)) -> Int {
  nel.reduce(l, fn(a, b) {
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
  let selling = remove_first_occurrence(nel.to_list(market.selling), price)
  let market = exchange(market, price, time_idx)
  let selling = nel.new(offer_margin() + max(market.buying), selling)
  Market(..market, selling:)
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

  let minh = market.min_history |> nel.to_list
  let min_history =
    set_nth_element(
      minh,
      time_idx,
      min(nel.new(price, [nth_element(minh, time_idx)])),
    )
    |> nel.from_list

  let maxh = market.min_history |> nel.to_list
  let max_history =
    set_nth_element(
      maxh,
      time_idx,
      max(nel.new(price, [nth_element(minh, time_idx)])),
    )
    |> nel.from_list
  Market(
    market.buying,
    market.selling,
    max_history,
    min_history,
    last_price,
    delta,
  )
}

pub fn main() -> Nil {
  io.println("Hello from app_gleam!")
}
