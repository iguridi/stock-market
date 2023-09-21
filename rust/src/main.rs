use rand::Rng; // 0.8.0
use std::{cmp, fmt};

const TIME: i32 = 100;
const NUMBER_OF_ORDERS: i32 = 10;

fn random(from: i32, to: i32) -> i32 {
    // Generate random number in the range [from, to-1]
    rand::thread_rng().gen_range(from..to)
}

fn fifty_fifty() -> bool {
    rand::thread_rng().gen_bool(0.5)
}

trait Offering {
    fn get_orders(&mut self) -> &mut Vec<i32>;
    fn next_order(&self) -> i32;
    fn best_price(&self) -> i32;

    fn add_order(&mut self, order: i32) {
        self.get_orders().push(order);
    }
    fn remove_order(&mut self, order: i32) {
        let orders = self.get_orders();
        if let Some(index) = orders.iter().position(|&x| x == order) {
            orders.remove(index);
        } else {
            panic!("this should never happen");
        }
    }
    fn new_limit_order(&mut self, price: i32) {
        self.add_order(price);
    }
    fn market_order(&mut self, new_limit_order: i32) -> i32 {
        let price = self.best_price();
        self.remove_order(price);
        self.add_order(new_limit_order);
        price
    }

    fn ask_bid_spread() -> i32 {
        random(1, 12)
    }
}

struct Asking {
    orders: Vec<i32>,
}

struct Biding {
    orders: Vec<i32>,
}

impl Offering for Asking {
    fn get_orders(&mut self) -> &mut Vec<i32> {
        &mut self.orders
    }
    fn best_price(&self) -> i32 {
        match self.orders.iter().min() {
            Some(o) => o.to_owned(),
            None => panic!("this shouldn't happen"),
        }
    }
    fn next_order(&self) -> i32 {
        self.best_price() + Self::ask_bid_spread()
    }
}

impl Offering for Biding {
    fn get_orders(&mut self) -> &mut Vec<i32> {
        &mut self.orders
    }
    fn best_price(&self) -> i32 {
        match self.orders.iter().max() {
            Some(o) => o.to_owned(),
            None => panic!("this shouldn't happen"),
        }
    }
    fn next_order(&self) -> i32 {
        cmp::max(self.best_price() - Self::ask_bid_spread(), 1)
    }
}

struct History {
    min: Vec<i32>,
    max: Vec<i32>,
}

impl History {
    fn new() -> Self {
        Self {
            max: (0..TIME).map(|_| 0).collect(),
            min: (0..TIME).map(|_| 1000).collect(),
        }
    }
    fn register(&mut self, time: usize, price: i32) {
        self.min[time] = cmp::min(price, self.min[time]);
        self.max[time] = cmp::max(price, self.max[time]);
    }
    fn max(&self) -> i32 {
        cmp::max(
            self.min.iter().max().unwrap().to_owned(),
            self.max.iter().max().unwrap().to_owned(),
        )
    }
    fn range(&self, time: usize) -> (usize, usize) {
        (self.min[time] as usize, self.max[time] as usize)
    }
    fn chart(&self) {
        let mut chart = Chart::new(self.max());
        for turn in 0..TIME as usize {
            let (from, to) = self.range(turn);
            chart.draw_interval(from, to, turn);
        }
        print!("{}", chart)
    }
}

struct Chart<'a> {
    grid: Vec<Vec<&'a str>>,
    max: i32,
}

impl<'a> Chart<'a> {
    fn new(max: i32) -> Self {
        let grid = (0..max + 1)
            .map(|_| (0..TIME).map(|_| "  ").collect())
            .collect();
        Self { grid, max }
    }

    fn put(&mut self, price: usize, turn: usize, icon: &'a str) {
        let len = self.grid.len();
        self.grid[len - price][turn] = icon
    }

    fn draw_interval(&mut self, min: usize, max: usize, turn: usize) {
        for price in min..max {
            self.put(price, turn, " |");
        }
        self.put(min, turn, " +");
        self.put(max, turn, " +");
    }
}

impl<'a> fmt::Display for Chart<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\nprice({})", self.max)?;

        for row in &self.grid {
            writeln!(f, " | {}", row.concat())?;
        }

        let underscores = "_".repeat(2 * TIME as usize);
        write!(f, " |_{}__ time({})", underscores, TIME)
    }
}

struct Market {
    biding: Biding,
    asking: Asking,
    history: History,
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
            history: History::new(),
            last_price: first_price,
            delta: 0,
        }
    }
    fn ask(&mut self, time: usize) {
        // We get the next asking price not from other asks, but from
        // what the bids are, with a decent margin for earnings
        // This is not very realistic, but oh well
        let price = self.asking.market_order(self.biding.next_order());
        self.record_exchange(time, price);
    }

    fn bid(&mut self, time: usize) {
        let price = self.biding.market_order(self.asking.next_order());
        self.record_exchange(time, price);
    }

    fn record_exchange(&mut self, time: usize, price: i32) {
        self.delta = price - self.last_price;
        self.last_price = price;
        self.history.register(time, price);
    }

    fn turn(&mut self, turn: usize) {
        let delta = self.delta;
        if fifty_fifty() {
            self.ask(turn);
        } else {
            self.bid(turn);
        }
        // Make it swing more
        if delta < 0 {
            self.ask(turn);
            self.ask(turn);
        }
        if delta > 0 {
            self.bid(turn);
            self.bid(turn);
        }
    }

    fn chart(&self) {
        self.history.chart()
    }
}

fn main() {
    let mut market = Market::new();
    for turn in 0..TIME as usize {
        market.turn(turn)
    }
    market.chart();
}
