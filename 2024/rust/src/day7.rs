use std::result;

static FILE_PATH: &'static str = "../data/day7.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 7");
    part1(&contents);
    part2(&contents);
}

#[derive(Debug)]
enum Operator {
    Plus,
    Mul,
    Concat,
}

struct Equation {
    solution: u64,
    values: Vec<u64>,
}

impl Equation {
    fn is_solvable(&self) -> bool {
        // Reinventing the wheel here
        // Generate all possibilities by using a bitmap
        let num_operators = (self.values.len() - 1) as u32;
        let possibilities = 2_u32.pow(num_operators);
        for iteration in 0..possibilities {
            let operators = (0..possibilities).map(|v| 1 << v & iteration != 0);
            let mut result = self.values[0];
            for (val, op) in self.values[1..].iter().zip(operators) {
                if op {
                    result += val
                } else {
                    result *= val
                };
                if result > self.solution {
                    break;
                }
            }
            if result == self.solution {
                return true;
            }
        }
        false
    }

    fn is_solvable2(&self) -> bool {
        // What a crazy solution lol
        // I am tracking possible combinations by counting in base 3
        // 0 -> plus, 1 -> multiply, 2 -> concat
        let num_operators = (self.values.len() - 1) as u32;
        let possibilities = 3_u32.pow(num_operators);
        for iteration in 0..possibilities {
            let operators = (0..num_operators).map(|v| {
                let rem = iteration.rem_euclid(3_u32.pow(v + 1));
                if rem >= 2 * 3_u32.pow(v) {
                    Operator::Concat
                } else if rem >= 3_u32.pow(v) {
                    Operator::Mul
                } else {
                    Operator::Plus
                }
            });
            let mut result = self.values[0];
            for (val, op) in self.values[1..].iter().zip(operators) {
                match op {
                    Operator::Plus => result += val,
                    Operator::Mul => result *= val,
                    Operator::Concat => {
                        result = result * 10_u64.pow(val.ilog10() + 1) + val;
                    }
                }
                if result > self.solution {
                    break;
                }
            }
            if result == self.solution {
                return true;
            }
        }
        false
    }
}

fn read_data(contents: &str) -> Vec<Equation> {
    contents
        .lines()
        .map(|l| {
            let (solution, values) = l.split_once(":").unwrap();
            let value_vec = values
                .trim()
                .split(" ")
                .map(|v| v.parse().unwrap())
                .collect();
            Equation {
                solution: solution.trim().parse().unwrap(),
                values: value_vec,
            }
        })
        .collect()
}

fn part1(contents: &str) -> u64 {
    let equations = read_data(contents);
    let answer = equations
        .iter()
        .filter(|e| e.is_solvable())
        .map(|e| e.solution)
        .sum::<u64>();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u64 {
    let equations = read_data(contents);
    let answer = equations
        .iter()
        .filter(|e| e.is_solvable2())
        .map(|e| e.solution)
        .sum::<u64>();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 3749);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 11387);
    }
}

