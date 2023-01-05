from random import randint, random

TIME = 100
NUMBER_OF_OFFERS = 10

def offer_margin(): return randint(0, 10)

class Market:
    buying = [randint(4, 25) for _ in range(NUMBER_OF_OFFERS)]
    selling = [randint(25, 50) for _ in range(NUMBER_OF_OFFERS)]

    max_history = [0 for _ in range(TIME)]
    min_history = [1000 for _ in range(TIME)]

    last_price = min(selling)
    delta = 0

    @property
    def buy_price(self): return min(self.selling)

    @property
    def sell_price(self): return max(self.buying)

    def buy(self, time):
        price = self.buy_price
        self.selling.remove(price)
        self._exchange(price, time)
        self._new_sell_offer()

    def sell(self, time):
        price = self.sell_price
        self.buying.remove(price)
        self._exchange(price, time)
        self._new_buy_offer()

    def _exchange(self, price, time):
        self.delta = price - self.last_price
        self.last_price = price
        self.min_history[time] = min(price, self.min_history[time])
        self.max_history[time] = max(price, self.max_history[time])

    def _new_buy_offer(self):
        self.buying.append(max(self.buy_price - offer_margin(), 1))

    def _new_sell_offer(self):
        self.selling.append(offer_margin() + self.sell_price)

def interval_graph(market):
    max_history = market.max_history
    min_history = market.min_history
    price = max(*max_history, *min_history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        min_price = min_history[t]
        max_price = max_history[t]
        for p in range(min_price, max_price): grid[-p][t] = "|"
        grid[-min_price][t] = "+"
        grid[-max_price][t] = "+"
    print(f"\nprice({price})")
    for string in ["".join(i) for i in grid]: print("  |", string)
    print("  |_" + "_" * TIME + f"__ time({TIME})")

m = Market()

for t in range(TIME):
    delta = m.delta
    if random() < 0.5:
        m.buy(t)
    else:
        m.sell(t)
    if delta < 0:
        m.buy(t)
        m.buy(t)
    if delta > 0:
        m.sell(t)
        m.sell(t)

interval_graph(m)
