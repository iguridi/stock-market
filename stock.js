const randomInt = (min, max) =>
  Math.floor(Math.random() * (max - min + 1)) + min;

const offerMargin = () => randomInt(0, 10);

const genArrayRandom = (length, min, max) =>
  Array.from({ length }, () => randomInt(min, max));

const genArray = (length, number) => Array.from({ length }, () => number);

const TIME = 150;
const NUMBER_OF_OFFERS = 10;

const market = {
  buying: genArrayRandom(NUMBER_OF_OFFERS, 4, 25),
  selling: genArrayRandom(NUMBER_OF_OFFERS, 25, 50),

  maxHistory: genArray(TIME, 0),
  minHistory: genArray(TIME, 1000),

  lastPrice: null,
  delta: null,

  buyPrice() {
    return Math.min(...this.selling);
  },

  buy(time) {
    const price = this.buyPrice();
    const position = this.selling.indexOf(price);
    this.selling = this.selling.filter((_, i) => i != position);
    this._exchange(price, time);
    this._new_sell_offer();
  },

  sell(time) {
    const price = this.sellPrice();
    const position = this.buying.indexOf(price);
    this.buying = this.buying.filter((_, i) => i != position);
    this._exchange(price, time);
    this._new_buy_offer();
  },

  sellPrice() {
    return Math.max(...this.buying);
  },

  _exchange(price, time) {
    if (this.lastPrice !== null) {
      this.delta = price - this.lastPrice;
    }
    this.lastPrice = price;
    this.minHistory[time] = Math.min(price, this.minHistory[time]);
    this.maxHistory[time] = Math.max(price, this.maxHistory[time]);
  },

  _new_buy_offer() {
    this.buying.push(Math.max(this.buyPrice() - offerMargin(), 1));
  },

  _new_sell_offer() {
    this.selling.push(offerMargin() + this.sellPrice());
  },
};

const intervalGraph = (market) => {
  const maxHistory = market.maxHistory;
  const minHistory = market.minHistory;
  const price = Math.max(...maxHistory, ...minHistory);
  const grid = [];
  for (let i = 0; i < price; i++) {
    grid.push([]);
    for (let j = 0; j < TIME; j++) {
      grid[i].push(" ");
    }
  }
  for (let t = 0; t < TIME; t++) {
    const minPrice = minHistory[t];
    const maxPrice = maxHistory[t];
    for (let p = minPrice; p < maxPrice; p++) {
      grid[price - p][t] = "|";
    }
    grid[price - minPrice][t] = "+";
    grid[price - maxPrice][t] = "+";
  }
  console.log(`\nprice(${price})`);
  for (let i = 0; i < grid.length; i++) {
    const row = grid[i];
    const stringRow = row.join("");
    console.log(" |", stringRow);
  }
  console.log(" |_" + "_".repeat(TIME) + `__ time(${TIME})`);
};

const m = market;
for (let t = 0; t < TIME; t++) {
  const delta = m.delta;
  if (Math.random() < 0.5) {
    m.buy(t);
  } else {
    m.sell(t);
  }
  if (delta === null) {
    continue;
  }
  if (delta < 0) {
    m.buy(t);
    m.buy(t);
  }
  if (delta > 0) {
    m.sell(t);
    m.sell(t);
  }
}

intervalGraph(m);
