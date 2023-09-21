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

    def buy(self, time):
        price = min(self.selling)
        self.selling.remove(price)
        self._exchange(price, time)
        self._new_sell_offer()

    def sell(self, time):
        price = max(self.buying)
        self.buying.remove(price)
        self._exchange(price, time)
        self._new_buy_offer()

    def _exchange(self, price, time):
        self.delta = price - self.last_price
        self.last_price = price
        self.min_history[time] = min(price, self.min_history[time])
        self.max_history[time] = max(price, self.max_history[time])

    def _new_buy_offer(self): self.buying.append(max(min(self.selling) - offer_margin(), 1))

    def _new_sell_offer(self): self.selling.append(offer_margin() + max(self.buying))

def interval_graph(m):
    # TODO: ugly code
    price = max(*m.max_history, *m.min_history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        min_price = m.min_history[t]
        max_price = m.max_history[t]
        for p in range(min_price, max_price): grid[-p][t] = "|"
        grid[-min_price][t] = "+"
        grid[-max_price][t] = "+"
    print(f"\nprice({price})")
    for string in ["".join(i) for i in grid]: print("  |", string)
    print("  |_" + "_" * TIME + f"__ time({TIME})")

m = Market()

for t in range(TIME):
    decision = m.buy if random() < 0.5 else m.sell
    tendency = m.buy if m.delta < 0.5 else m.sell
    [f(t) for f in (decision, tendency, tendency)]

interval_graph(m)
