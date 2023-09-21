use rand::Rng; // 0.8.0
use std::cmp;

const TIME: i32 = 100;
const NUMBER_OF_ORDERS: i32 = 10;

fn random(from: i32, to: i32) -> i32 {
    // Generate random number in the range [from, to-1]
    rand::thread_rng().gen_range(from..to)
}

fn ask_bid_spread() -> i32 {
    random(1, 12)
}

fn remove_from_vector(vec: &Vec<i32>, value: i32) -> Vec<i32> {
    let pos = vec
        .iter()
        .position(|x| *x == value)
        .expect("value not found");
    [&vec[..pos], &vec[pos + 1..]].concat()
}

trait Offering {
    fn get_orders(&self) -> &Vec<i32>;
    fn add_order(self: &mut Self, order: i32);
    fn remove_order(self: &mut Self, order: i32);
    fn best_price(&self) -> i32;
    fn new_limit_order(self: &mut Self, price: i32);
    fn market_order(self: &mut Self, new_limit_order: i32) -> i32 {
        let price = self.best_price();
        self.remove_order(price);
        self.add_order(new_limit_order);
        price
    }
}

#[derive(Debug)]
struct Asking {
    orders: Vec<i32>,
}

#[derive(Debug)]
struct Biding {
    orders: Vec<i32>,
}

impl Offering for Asking {
    fn get_orders(&self) -> &Vec<i32> {
        &self.orders
    }
    fn add_order(self: &mut Self, order: i32) {
        self.orders.push(order);
    }
    fn remove_order(self: &mut Self, order: i32) {
        self.orders = remove_from_vector(&self.orders, order);
    }
    fn best_price(&self) -> i32 {
        self.orders.iter().min().unwrap().to_owned()
    }
    fn new_limit_order(self: &mut Self, price: i32) {
        self.add_order(price);
    }
}

impl Offering for Biding {
    fn get_orders(&self) -> &Vec<i32> {
        &self.orders
    }
    fn add_order(self: &mut Self, order: i32) {
        self.orders.push(order);
    }
    fn remove_order(self: &mut Self, order: i32) {
        self.orders = remove_from_vector(&self.orders, order);
    }
    fn best_price(&self) -> i32 {
        self.orders.iter().max().unwrap().to_owned()
    }
    fn new_limit_order(self: &mut Self, price: i32) {
        self.add_order(price);
    }
}

#[derive(Debug)]
struct Market {
    biding: Biding,
    asking: Asking,
    max_history: Vec<i32>,
    min_history: Vec<i32>,
    last_price: i32,
    delta: i32,
}

impl Market {
    fn new() -> Self {
        let orders = (0..NUMBER_OF_ORDERS).map(|_x| random(4, 25)).collect();
        let biding = Biding { orders };
        let orders = (0..NUMBER_OF_ORDERS).map(|_x| random(25, 50)).collect();
        let asking = Asking { orders };
        let first_price = biding.best_price();

        Self {
            biding,
            asking,
            max_history: (0..TIME).map(|_| 0).collect(),
            min_history: (0..TIME).map(|_| 1000).collect(),
            last_price: first_price,
            delta: 0,
        }
    }
    fn buy_market_order(self: &mut Self, time: usize) {
        let new_limit_order = cmp::max(self.biding.best_price() - ask_bid_spread(), 1);
        let price = self.asking.market_order(new_limit_order);
        self.record_exchange(time, price);
    }

    fn sell_market_order(self: &mut Self, time: usize) {
        let new_limit_order = self.asking.best_price() + ask_bid_spread();
        let price = self.biding.market_order(new_limit_order);
        self.record_exchange(time, price);
    }

    fn record_exchange(self: &mut Self, time: usize, price: i32) {
        self.delta = price - self.last_price;
        self.last_price = price;
        self.min_history[time] = cmp::min(price, self.min_history[time]);
        self.max_history[time] = cmp::max(price, self.max_history[time]);
    }
}

fn interval_graph(market: &Market) {
    let max_history = &market.max_history;
    let min_history = &market.min_history;
    let price = cmp::max(
        max_history.iter().max().unwrap().to_owned(),
        min_history.iter().max().unwrap().to_owned(),
    );
    let mut grid: Vec<Vec<&str>> = (0..price + 1)
        .map(|_| (0..TIME).map(|_| "  ").collect())
        .collect();
    let grid_length = &grid.len();
    for turn in 0..TIME as usize {
        let min_price = min_history[turn] as usize;
        let max_price = max_history[turn] as usize;
        for price in min_price..max_price {
            grid[grid_length - price][turn] = " |";
        }
        grid[grid_length - min_price][turn] = " +";
        grid[grid_length - max_price][turn] = " +";
    }
    print!("\nprice({})\n", price);
    for row in &grid {
        let string_repr = row.join("");
        print!(" | {}\n", string_repr);
    }
    print!(" |_{}__ time({}", str::repeat("_", 2 * TIME as usize), TIME);
}

fn main() {
    let mut market = Market::new();

    for turn in 0..TIME as usize {
        let delta = market.delta;
        if rand::thread_rng().gen_bool(0.5) {
            market.buy_market_order(turn);
        } else {
            market.sell_market_order(turn);
        }
        if delta < 0 {
            market.buy_market_order(turn);
            market.buy_market_order(turn);
        }
        if delta > 0 {
            market.sell_market_order(turn);
            market.sell_market_order(turn);
        }
    }

    interval_graph(&market);
}
