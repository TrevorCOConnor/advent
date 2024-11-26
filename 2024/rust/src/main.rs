mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;


use std::env;

fn main() {
    let args: Vec<String> = env::args().map(|x| x.to_lowercase()).collect();
    // Ignore target argument
    let args = &args[1..];


    if args.contains(&String::from("day1")) || args.is_empty() {
        day1::solutions();
    }

    if args.contains(&String::from("day2")) || args.is_empty() {
        day2::solutions();
    }

    if args.contains(&String::from("day3")) || args.is_empty() {
        day3::solutions();
    }

    if args.contains(&String::from("day4")) || args.is_empty() {
        day4::solutions();
    }

    if args.contains(&String::from("day5")) || args.is_empty() {
        day5::solutions();
    }

    if args.contains(&String::from("day6")) || args.is_empty() {
        day6::solutions();
    }

    if args.contains(&String::from("day7")) || args.is_empty() {
        day7::solutions();
    }

    if args.contains(&String::from("day8")) || args.is_empty() {
        day8::solutions();
    }

    if args.contains(&String::from("day9")) || args.is_empty() {
        day9::solutions();
    }

    if args.contains(&String::from("day10")) || args.is_empty() {
        day10::solutions();
    }

    if args.contains(&String::from("day11")) || args.is_empty() {
        day11::solutions();
    }

    if args.contains(&String::from("day12")) || args.is_empty() {
        day12::solutions();
    }

    if args.contains(&String::from("day13")) || args.is_empty() {
        day13::solutions();
    }

    if args.contains(&String::from("day14")) || args.is_empty() {
        day14::solutions();
    }

    if args.contains(&String::from("day15")) || args.is_empty() {
        day15::solutions();
    }

    if args.contains(&String::from("day16")) || args.is_empty() {
        day16::solutions();
    }

    if args.contains(&String::from("day17")) || args.is_empty() {
        day17::solutions();
    }

    if args.contains(&String::from("day18")) || args.is_empty() {
        day18::solutions();
    }

    if args.contains(&String::from("day19")) || args.is_empty() {
        day19::solutions();
    }

    if args.contains(&String::from("day20")) || args.is_empty() {
        day20::solutions();
    }

    if args.contains(&String::from("day21")) || args.is_empty() {
        day21::solutions();
    }

    if args.contains(&String::from("day22")) || args.is_empty() {
        day22::solutions();
    }

    if args.contains(&String::from("day23")) || args.is_empty() {
        day23::solutions();
    }

    if args.contains(&String::from("day24")) || args.is_empty() {
        day24::solutions();
    }

    if args.contains(&String::from("day25")) || args.is_empty() {
        day25::solutions();
    }
}
