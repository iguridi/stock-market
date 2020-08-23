from random import randint, random


def gen_sell_price():
    return randint(3, 23)


def gen_buy_price():
    return randint(0, 20)


TIME = 150


class Market:
    buying = [gen_buy_price() for _ in range(5)]
    selling = [gen_sell_price() for _ in range(5)]

    max_history = [0 for _ in range(TIME)]
    min_history = [100 for _ in range(TIME)]

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
        price = self.buy_price - gen_buy_price() // 2
        self.buying.append(price)

    def new_sell_offer(self):
        price = gen_sell_price() // 2 + self.sell_price
        self.selling.append(price)


def interval_graph(market):
    max_history = market.max_history
    min_history = market.min_history
    time = len(max_history)
    price = max(*max_history, *min_history)
    grid = [[" " for _ in range(time)] for _ in range(price)]
    for t in range(time):
        min_price = min_history[t]
        max_price = max_history[t]
        for p in range(min_price, max_price):
            grid[-p][t] = "|"
        grid[-min_price][t] = "+"
        grid[-max_price][t] = "+"

    print("\nprice")
    for string in ["".join(i) for i in grid]:
        print("  |", string)
    print("  |_" + "_" * time + "__ time")


m = Market()

for t in range(TIME):
    if random() < 0.5:
        m.new_sell_offer()
        m.buy(t)
    else:
        m.new_buy_offer()
        m.sell(t)
    print(m.delta)
    if m.delta is not None and m.delta > 0:
        m.new_sell_offer()
        m.buy(t)
    else:
        m.new_buy_offer()
        m.sell(t)


print(m.max_history)
print(m.min_history)
interval_graph(m)
