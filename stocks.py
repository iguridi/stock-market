from random import randint, random


# Assumptions
# NUMBER_OF_OFFERS sell orders always
# NUMBER_OF_OFFERS buy orders always
# First 10 buy orders are random between 4 and 25
# First 10 sell orders are random between 25 and 50
# If final exchange price last day was higher than the previous one, buy twice
# If final exchange price last day was lower than the previous one, sell twice
# Every time someone buys, a new selling offer is made, at a higher price than the current sell price
# Every time someone sells, a new buying offer is made, at a lower price than the current buy price


def offer_margin():
    return randint(0, 10)


TIME = 100
NUMBER_OF_OFFERS = 10


class Market:
    buying = [randint(4, 25) for _ in range(NUMBER_OF_OFFERS)]
    selling = [randint(25, 50) for _ in range(NUMBER_OF_OFFERS)]

    max_history = [0 for _ in range(TIME)]
    min_history = [1000 for _ in range(TIME)]

    history = [0 for _ in range(TIME)]

    last_price = None
    delta = None

    @property
    def buy_price(self):
        return min(self.selling)

    @property
    def sell_price(self):
        return max(self.buying)

    def buy(self, time):
        price = self.buy_price
        self.selling.remove(price)
        self._exchange(price, time)

    def sell(self, time):
        price = self.sell_price
        self.buying.remove(price)
        self._exchange(price, time)

    def _exchange(self, price, time):
        if self.last_price is not None:
            self.delta = price - self.last_price
        self.last_price = price
        self.min_history[time] = min(price, self.min_history[time])
        self.max_history[time] = max(price, self.max_history[time])
        self.history[time] = price

    def new_buy_offer(self):
        price = max(self.buy_price - offer_margin(), 1)
        self.buying.append(price)

    def new_sell_offer(self):
        price = offer_margin() + self.sell_price
        self.selling.append(price)


def interval_graph(market):
    max_history = market.max_history
    min_history = market.min_history
    price = max(*max_history, *min_history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        min_price = min_history[t]
        max_price = max_history[t]
        for p in range(min_price, max_price):
            grid[-p][t] = "|"
        grid[-min_price][t] = "+"
        grid[-max_price][t] = "+"

    print("\nprice")
    for string in ["".join(i) for i in grid]:
        print("  |", string)
    print("  |_" + "_" * TIME + "__ time")


def last_price_graph(market):
    history = market.history
    price = max(history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        p = history[t]
        grid[-p][t] = "*"

    print(f"\nprice({price})")
    for string in ["".join(i) for i in grid]:
        print("  |", string)
    print("  |_" + "_" * TIME + "__ time")


m = Market()

# print("History")
for t in range(TIME):
    delta = m.delta
    # print("| buy price:", m.buy_price, "| sell price:", m.sell_price, "| delta:", delta)
    if random() < 0.5:
        m.buy(t)
        m.new_sell_offer()
    else:
        m.sell(t)
        m.new_buy_offer()
    if delta is None:
        continue
    if delta < 0:
        m.buy(t)
        m.new_sell_offer()
        m.buy(t)
        m.new_sell_offer()
    if delta > 0:
        m.sell(t)
        m.new_buy_offer()
        m.sell(t)
        m.new_buy_offer()


interval_graph(m)
print()
last_price_graph(m)
