use rand::Rng; // 0.8.0
use std::cmp;

fn random(from: i32, to: i32) -> i32 {
    // Generate random number in the range [from, to-1]
    rand::thread_rng().gen_range(from..to)
}

fn offer_margin() -> i32 {
    random(0, 10)
}

fn remove_from_vector(vec: &Vec<i32>, value: i32) -> Vec<i32> {
    let pos = vec
        .iter()
        .position(|x| *x == value)
        .expect("value not found");
    [&vec[..pos], &vec[pos + 1..]].concat()
}

const TIME: i32 = 75;
const NUMBER_OF_OFFERS: i32 = 10;

#[derive(Debug)]
struct Market {
    buying: Vec<i32>,
    selling: Vec<i32>,
    max_history: Vec<i32>,
    min_history: Vec<i32>,
    last_price: Option<i32>,
    delta: Option<i32>,
}

impl Market {
    fn buy_price(&self) -> i32 {
        self.selling.iter().min().unwrap().to_owned()
    }

    fn sell_price(&self) -> i32 {
        self.buying.iter().max().unwrap().to_owned()
    }

    fn buy(self: &mut Self, time: usize) {
        let price = self.buy_price();
        self.selling = remove_from_vector(&self.selling, price);
        self._exchange(time, price);
        self._new_sell_offer();
    }

    fn sell(self: &mut Self, time: usize) {
        let price = self.sell_price();
        self.buying = remove_from_vector(&self.buying, price);
        self._exchange(time, price);
        self._new_buy_offer();
    }

    fn _exchange(self: &mut Self, time: usize, price: i32) {
        if let Some(i) = self.last_price {
            self.delta = Some(price - i)
        };
        self.last_price = Some(price);
        self.min_history[time] = cmp::min(price, self.min_history[time]);
        self.max_history[time] = cmp::max(price, self.max_history[time]);
    }

    fn _new_buy_offer(self: &mut Self) {
        self.buying
            .push(cmp::max(self.buy_price() - offer_margin(), 1));
    }

    fn _new_sell_offer(self: &mut Self) {
        self.selling.push(self.sell_price() + offer_margin());
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
        grid[grid_length - min_price + 1][turn] = " +";
        grid[grid_length - min_price][turn] = " |";
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
    let mut market = Market {
        buying: (0..NUMBER_OF_OFFERS).map(|_x| random(4, 25)).collect(),
        selling: (0..NUMBER_OF_OFFERS).map(|_x| random(25, 50)).collect(),
        max_history: (0..TIME).map(|_| 0).collect(),
        min_history: (0..TIME).map(|_| 1000).collect(),
        last_price: None,
        delta: None,
    };

    for turn in 0..TIME as usize {
        let delta = market.delta;
        if rand::thread_rng().gen_bool(0.5) {
            market.buy(turn);
        } else {
            market.sell(turn);
        }
        if let Some(value) = delta {
            if value < 0 {
                market.buy(turn);
                market.buy(turn);
            }
            if value > 0 {
                market.sell(turn);
                market.sell(turn);
            }
        }
    }

    interval_graph(&market);
}
