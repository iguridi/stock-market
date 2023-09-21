require 'securerandom'


TIME = 100
NUMBER_OF_ORDERS = 10


def random(from, to)
  SecureRandom.random_number(to - from) + from
end

def ask_bid_spread
  random(1, 12)
end


module Offering
  def initialize(orders)
    @orders = orders
  end

  def get_orders
    @orders
  end

  def add_order(order)
    @orders << order
  end

  def remove_order(order)
    index = @orders.index(order)
    if index.nil?
        raise "order doesn't exist somehow"
    else
        @orders.delete_at(index)
    end
  end

  def best_price
    raise "not implemented"
  end

  def new_limit_order(price)
    add_order(price)
  end

  def market_order(new_limit_order)
    price = best_price
    remove_order(price)
    add_order(new_limit_order)
    price
  end
end

class Asking
  include Offering

  def best_price
    @orders.min
  end
end

class Biding
  include Offering

  def best_price
    @orders.max
  end
end

class Market
  attr_reader :delta

  def initialize
    @biding = Biding.new((0...NUMBER_OF_ORDERS).map { random(4, 25) })
    @asking = Asking.new((0...NUMBER_OF_ORDERS).map { random(25, 50) })
    @max_history = Array.new(TIME, 0)
    @min_history = Array.new(TIME, 1000)
    @last_price = @biding.best_price
    @delta = 0
  end

  def buy_market_order(time)
    new_limit_order = [@biding.best_price - ask_bid_spread, 1].max
    price = @asking.market_order(new_limit_order)
    record_exchange(time, price)
  end

  def sell_market_order(time)
    new_limit_order = @asking.best_price + ask_bid_spread
    price = @biding.market_order(new_limit_order)
    record_exchange(time, price)
  end

  def record_exchange(time, price)
    @delta = price - @last_price
    @last_price = price
    @min_history[time] = [@min_history[time], price].min
    @max_history[time] = [@max_history[time], price].max
  end

  def interval_graph
    max_history = @max_history
    min_history = @min_history
    max_price = [max_history.max, min_history.max].max

    grid = Array.new(max_price + 1) { Array.new(TIME, '  ') }

    (0...TIME).each do |turn|
      min_price = min_history[turn]
      max_price = max_history[turn]

      (min_price...max_price).each do |price|
        grid[grid.length - price - 1][turn] = ' |'
      end
      grid[grid.length - min_price - 1][turn] = ' +'
      grid[grid.length - max_price - 1][turn] = ' +'
    end

    puts "\nprice(#{max_price})"

    grid.each do |row|
      puts " | #{row.join}"
    end

    puts " |_#{'_' * (2 * TIME)}__ time(#{TIME})"
  end
end


market = Market.new

TIME.times do |turn|
  delta = market.delta
  if SecureRandom.random_number < 0.5
    market.buy_market_order(turn)
  else
    market.sell_market_order(turn)
  end

  if delta < 0
    market.buy_market_order(turn)
    market.buy_market_order(turn)
  end

  if delta > 0
    market.sell_market_order(turn)
    market.sell_market_order(turn)
  end
end

market.interval_graph
