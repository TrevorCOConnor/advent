use std::collections::{HashMap, HashSet};

use rust::{Direction, Matrix, Position};

static FILE_PATH: &'static str = "../data/day10.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 10");
    part1(&contents);
    part2(&contents);
}

fn read_data(contents: &str) -> Matrix<u32> {
    Matrix::new(
        contents
            .lines()
            .map(|line| line.chars().filter_map(|num| num.to_digit(10)).collect())
            .collect(),
    )
}

// Help with readability
#[derive(PartialEq, Eq)]
enum Unique {
    Yes,
    No,
}

struct Traverser {
    explored: HashSet<Position>,
}

impl Traverser {
    fn new() -> Self {
        Traverser {
            explored: HashSet::new(),
        }
    }

    fn explore(
        &mut self,
        cache: &mut HashMap<Position, u32>,
        matrix: &Matrix<u32>,
        pos: &Position,
        count_unique: &Unique,
    ) -> u32 {
        // To count unique paths, we do not keep track of where we have been
        // Unique::No for part i
        // Unique::Yes for part ii
        if *count_unique == Unique::No {
            self.explored.insert(*pos);
        }
        let current_val = matrix.get(pos).unwrap();
        if *current_val == 9 {
            return 1;
        }
        let next_positions: Vec<Position> = [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
        .filter_map(|dir| pos.apply_dir(dir))
        .filter(|new_pos| {
            (!self.explored.contains(new_pos) || *count_unique == Unique::Yes)
                && matrix
                    .get(new_pos)
                    .is_some_and(|&val| val == current_val + 1)
        })
        .collect();

        let mut sum = 0;
        for new_pos in next_positions {
            let val = self.explore(cache, matrix, &new_pos, &count_unique);
            cache.insert(new_pos, val);
            sum += val;
        }
        sum
    }
}

fn part1(contents: &str) -> u32 {
    let matrix = read_data(contents);
    let starting_points: Vec<Position> = matrix
        .values()
        .iter()
        .filter(|(_, &val)| val == 0)
        .map(|(pos, _)| *pos)
        .collect();

    let mut cache = HashMap::new();
    let mut sum = 0;
    for pos in starting_points {
        let mut traverser = Traverser::new();
        let val = traverser.explore(&mut cache, &matrix, &pos, &Unique::No);
        sum += val;
    }
    let answer = sum;
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let matrix = read_data(contents);
    let starting_points: Vec<Position> = matrix
        .values()
        .iter()
        .filter(|(_, &val)| val == 0)
        .map(|(pos, _)| *pos)
        .collect();

    let mut cache = HashMap::new();
    let mut sum = 0;
    for pos in starting_points {
        let mut traverser = Traverser::new();
        let val = traverser.explore(&mut cache, &matrix, &pos, &Unique::Yes);
        sum += val;
    }
    let answer = sum;
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 36);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 81);
    }
}
