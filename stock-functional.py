from collections import namedtuple
from random import randint, random

TIME = 100
NUMBER_OF_OFFERS = 10

History = namedtuple("History", ["min_history", "max_history", "last_price", "delta"])


def offer_margin():
    return randint(0, 10)


def buy(time, buying, selling, history):
    buy_price = min(selling)
    selling = remove(selling, buy_price)
    selling = add_sell_offer(selling, max(buying))
    history = write_history(buy_price, time, history)
    return buying, selling, history


def sell(time, buying, selling, history):
    sell_price = max(buying)
    buying = remove(buying, sell_price)
    buying = add_buy_offer(buying, min(selling))
    history = write_history(sell_price, time, history)
    return buying, selling, history


def remove(values, value):
    index = values.index(value)
    return values[:index] + values[index + 1 :]


def add_buy_offer(buying, price):
    return buying + (max(price - offer_margin(), 1),)


def add_sell_offer(selling, price):
    return selling + (offer_margin() + price,)


def write_history(price, time, h):
    delta = None
    if h.last_price is not None:
        delta = price - h.last_price
    return History(
        min_history=(
            h.min_history[:time]
            + (min(price, h.min_history[time]),)
            + h.min_history[time + 1 :]
        ),
        max_history=(
            h.max_history[:time]
            + (max(price, h.max_history[time]),)
            + h.max_history[time + 1 :]
        ),
        last_price=price,
        delta=delta,
    )


buying = tuple(randint(4, 25) for _ in range(NUMBER_OF_OFFERS))
selling = tuple(randint(25, 50) for _ in range(NUMBER_OF_OFFERS))

history = History(
    max_history=tuple(0 for _ in range(TIME)),
    min_history=tuple(1000 for _ in range(TIME)),
    last_price=None,
    delta=None,
)

last_price = None


def interval_graph(history):
    max_history = history.max_history
    min_history = history.min_history
    price = max(*max_history, *min_history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        min_price = min_history[t]
        max_price = max_history[t]
        for p in range(min_price, max_price):
            grid[-p][t] = "|"
        grid[-min_price][t] = "+"
        grid[-max_price][t] = "+"

    print(f"\nprice({price})")
    for string in ["".join(i) for i in grid]:
        print("  |", string)
    print("  |_" + "_" * TIME + f"__ time({TIME})")


def print_state(history, time):
    print(history.min_history[time], history.max_history[time], history.delta)


for t in range(TIME):
    delta = history.delta
    if random() < 0.5:
        buying, selling, history = buy(t, buying, selling, history)
    else:
        buying, selling, history = sell(t, buying, selling, history)
    if delta is None:
        continue
    if delta < 0:
        buying, selling, history = buy(t, buying, selling, history)
        buying, selling, history = buy(t, buying, selling, history)
    if delta > 0:
        buying, selling, history = sell(t, buying, selling, history)
        buying, selling, history = sell(t, buying, selling, history)
    # print_state(history, t)

interval_graph(history)
