mod day1;
mod day2;
mod day3;

mod day12;

use std::env;

fn main() {
    let args: Vec<String> = env::args().map(|x| x.to_lowercase()).collect();
    // Ignore target argument
    let args = &args[1..];
    if args.contains(&String::from("day1")) || args.is_empty() {
        day1::day1();
    }
    if args.contains(&String::from("day2")) || args.is_empty() {
        day2::day2();
    }
}
