use rust::*;
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().map(|x| x.to_lowercase()).collect();
    // Ignore target argument
    let args = &args[1..];

    if args.contains(&String::from("day1")) || args.is_empty() {
        day1::solution()?;
    }
    if args.contains(&String::from("day2")) || args.is_empty() {
        day2::solution()?;
    }

    Ok(())
}
