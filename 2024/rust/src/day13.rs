static FILE_PATH: &'static str = "../data/day13.txt";
use std::ops::Add;

use regex::{self, Regex};

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 13");
    part1(&contents);
    part2(&contents);
}

#[derive(Debug, PartialEq)]
struct Vector3(f64, f64, f64);

impl Vector3 {
    fn new(a: f64, b: f64, c: f64) -> Self {
        Vector3(a, b, c)
    }

    fn scale(&self, scalar: f64) -> Self {
        Vector3(self.0 * scalar, self.1 * scalar, self.2 * scalar)
    }
}

impl Add for Vector3 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Vector3(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2)
    }
}

#[derive(Debug)]
struct Machine {
    button_a: (f64, f64),
    button_b: (f64, f64),
    prize: (f64, f64),
}

fn read_data(contents: &str) -> Vec<Machine> {
    let re = Regex::new(
        r"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)\n\s*Button B: X\+(?<bx>\d+), Y\+(?<by>\d+)\n\s*Prize: X=(?<x_prize>\d+), Y=(?<y_prize>\d+)",
    )
    .unwrap();

    re.captures_iter(contents)
        .map(|mtch| Machine {
            button_a: (mtch["ax"].parse().unwrap(), mtch["ay"].parse().unwrap()),
            button_b: (mtch["bx"].parse().unwrap(), mtch["by"].parse().unwrap()),
            prize: (
                mtch["x_prize"].parse().unwrap(),
                mtch["y_prize"].parse().unwrap(),
            ),
        })
        .collect()
}

fn solve_system_of_equations(machine: &Machine) -> Option<(f64, f64)> {
    let vec_a = Vector3::new(machine.button_a.0, machine.button_b.0, machine.prize.0);
    let vec_b = Vector3::new(machine.button_a.1, machine.button_b.1, machine.prize.1);

    let scalar = -1. * (machine.button_b.0 / machine.button_b.1);
    let reduced_vec_a = (vec_a + vec_b.scale(scalar))
        .scale((machine.button_a.0 + machine.button_a.1 * scalar).recip());

    let scalar = -1. * machine.button_a.1;
    let reduced_vec_b = (vec_b + reduced_vec_a.scale(scalar)).scale(machine.button_b.1.recip());

    let solution = (reduced_vec_a.2.round(), reduced_vec_b.2.round());

    let vec_a_check = Vector3::new(machine.button_a.0, machine.button_a.1, 0.);
    let vec_b_check = Vector3::new(machine.button_b.0, machine.button_b.1, 0.);
    let prize_check = Vector3::new(machine.prize.0, machine.prize.1, 0.);

    if (vec_a_check.scale(solution.0) + vec_b_check.scale(solution.1)) == prize_check {
        Some(solution)
    } else {
        None
    }
}

fn part1(contents: &str) -> f64 {
    let machines = read_data(contents);
    let answer = machines
        .iter()
        .filter_map(solve_system_of_equations)
        .map(|(a, b)| a * 3. + b)
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> f64 {
    let machines = read_data(contents);
    let offset = 10000000000000.;
    let answer = machines
        .iter()
        .map(|machine| Machine {
            button_a: machine.button_a,
            button_b: machine.button_b,
            prize: (machine.prize.0 + offset, machine.prize.1 + offset),
        })
        .filter_map(|m| solve_system_of_equations(&m))
        .map(|(a, b)| a * 3. + b)
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::part1;

    static DATA: &'static str = "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 480.);
    }
}

