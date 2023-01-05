from collections import namedtuple
from random import randint, random
from functools import reduce

TIME = 100
NUMBER_OF_OFFERS = 10

def offer_margin(): return randint(0, 10)

def buy(buying, selling):
    selling, price = exchange(selling, buying, add_sell_offer, min, max)
    return buying, selling, price

def sell(buying, selling):
    buying, price = exchange(buying, selling, add_buy_offer, max, min)
    return buying, selling, price

def exchange(exchange_list, offer_list, add_offer, exchange_function, offer_function):
    price = exchange_function(exchange_list)
    exchange_list = remove(exchange_list, price)
    exchange_list = add_offer(exchange_list, offer_function(offer_list))
    return exchange_list, price

def make_action(time, buying, selling, history, action):
    buying, selling, price = action(buying, selling)
    history = write_history(price, time, history)
    return buying, selling, history

def remove(values, value):
    index = values.index(value)
    return values[:index] + values[index + 1 :]

def add_buy_offer(buying, price): return buying + (max(price - offer_margin(), 1),)
def add_sell_offer(selling, price): return selling + (offer_margin() + price,)

def write_history(price, time, h):
    return History(
        min_history=(h.min_history[:time] + (min(price, h.min_history[time]),) + h.min_history[time + 1 :]),
        max_history=(h.max_history[:time] + (max(price, h.max_history[time]),) + h.max_history[time + 1 :]),
        last_price=price,
        delta=price - h.last_price)

def interval_graph(history):
    # TODO: this is ugly
    max_history = history.max_history
    min_history = history.min_history
    price = max(*max_history, *min_history)
    grid = [[" " for _ in range(TIME)] for _ in range(price)]
    for t in range(TIME):
        for p in range(min_history[t], max_history[t]): grid[-p][t] = "|"
        grid[-min_history[t]][t] = "+"
        grid[-max_history[t]][t] = "+"

    print(f"\nprice({price})")
    for string in ["".join(i) for i in grid]: print("  |", string)
    print("  |_" + "_" * TIME + f"__ time({TIME})")

def print_state(history, time): print(history.min_history[time], history.max_history[time], history.delta)

def a_moment(info, t):
    delta = history.delta
    randomDecision = buy if random() < 0.5 else sell
    tendency = buy if delta < 0 else sell
    return reduce(lambda x, y: make_action(t, *x, y), [randomDecision, tendency, tendency], info)

buying = tuple(randint(1, 25) for _ in range(NUMBER_OF_OFFERS))
selling = tuple(randint(25, 50) for _ in range(NUMBER_OF_OFFERS))

last_price = min(selling)

History = namedtuple("History", ["min_history", "max_history", "last_price", "delta"])
history = History(
    max_history=tuple(0 for _ in range(TIME)),
    min_history=tuple(1000 for _ in range(TIME)),
    last_price=last_price,
    delta=0)

for t in range(TIME): buying, selling, history = a_moment((buying, selling, history), t)

interval_graph(history)
