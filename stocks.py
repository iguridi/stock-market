# from dataclasses import dataclass
from random import randint

# # Assumptions
# # - One type of stock
# # - One market
# # - No dividends, nor taxes
# # - The capital of investors is infinite
# # - Each broket has 3 possible actions every minute:
# #   set sell notice for a proportion of his stocks at some price
# #   set buy notice for a proportion of his stocks at some price
# #   do nothing


# STOCK_NUMBER = 1000
# STOCK_BROKER_NUMBER = 20
# SIMULATION_DAYS = 20
# SELL = "sell"
# BUY = "buy"
# NOTHING = "no_action"
# BROKER_OPTIONS = (SELL, BUY, NOTHING)
# OPTIONS_WEIGHT = (2, 2, 56)
# STOCK_PRICE = 100
# STOCK_MAX_DEVIATION = 50
# SELL_PROPORTIONS = (0, 1 / 3, 1 / 2)
# PROPORTIONS_WEIGHT = (50, 25, 25)
# MAX_STOCK_BUY_A_MINUTE_PER_BROKER = 5


# class Broker:
#     def __init__(self):
#         self.buy_order = None

#     def act(self, stocks):
#         action = choices(BROKER_OPTIONS, weights=OPTIONS_WEIGHT)
#         if action == SELL:
#             self._sell(list(stocks))
#         if action == BUY:
#             self._buy()

#     def _sell(self, stocks):
#         proportion = choices(SELL_PROPORTIONS, PROPORTIONS_WEIGHT)
#         number = int(len(stocks) / proportion)
#         for i in range(number):
#             stocks[i].price = self._stock_price()
#             stocks[i].selling = True

#     def _buy(self):
#         number = randint(0, MAX_STOCK_BUY_A_MINUTE_PER_BROKER)
#         self.buy_order = BuyOrder(number, self._stock_price())

#     def _stock_price(self):
#         return STOCK_PRICE + randint(-STOCK_MAX_DEVIATION, STOCK_MAX_DEVIATION)


# @dataclass
# class Stock:
#     owner: Broker = None
#     selling: int = False
#     price: int = None


# @dataclass
# class BuyOrder:
#     number: int = 0
#     price: int = None


# class Market:
#     def __init__(self):
#         self.stocks = [Stock() for _ in range(STOCK_NUMBER)]
#         self.brokers = [Broker() for _ in range(STOCK_BROKER_NUMBER)]
#         # Assign stocks
#         for s in self.stocks:
#             s.owner = self.brokers[randint(0, STOCK_BROKER_NUMBER)]

#     def _make_transactions(self):
#         pass
#         # Get stocks for sale
#         # Get buy orders
#         # For every buy order get one sell order, the first that works
#         # The transaction price will be the average between buying and selling
#         # Change stock ownership
#         # Stop selling stock
#         # Remove buy order

#     def receive_brokers_orders(self):
#         for b in self.brokers:
#             his_stocks = (s for s in self.stocks if s.owner == b)
#             b.act(his_stocks)


# class Simulation:
#     def __init__(self):
#         self.market = Market()
#         self.minute = 0

#     def run(self):
#         for minute in range(SIMULATION_DAYS * 24 * 60):
#             # Every minute

#             self.minute = minute


class Market:
    selling = [randint(3, 7) for _ in range(50)]
    buying = [randint(2, 6) for _ in range(50)]

    sell_history = []
    buy_history = []

    @property
    def buy_price(self):
        return min(self.selling)

    @property
    def sell_price(self):
        return max(self.buying)

    def buy(self):
        price = self.buy_price
        self.selling.remove(price)
        self.buy_history.append(price)

    def sell(self):
        price = self.sell_price
        self.buying.remove(price)
        self.sell_history.append(price)

    def new_buy_offer(self, price):
        self.buying = price

    def new_sell_offer(self, price):
        self.selling = price


def graph(market):
    buy_history = market.buy_history
    sell_history = market.sell_history
    time = len(buy_history)
    price = max(*buy_history, *sell_history)
    grid = [[" " for _ in range(time)] for _ in range(price)]
    for t in range(time):
        sell_price = sell_history[t]
        buy_price = buy_history[t]
        grid[1 - sell_price][t] = "*"
        grid[1 - buy_price][t] = "+"

    print("\nprice")
    for string in ["".join(i) for i in grid]:
        print("  |", string)
    print("  |_" + "_" * time + "__ time")


m = Market()

for i in range(50):
    m.buy()
    m.sell()

print(m.buy_history)
print(m.sell_history)
graph(m)
