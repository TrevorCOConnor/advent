use std::{
    collections::HashMap,
    ops::{Add, Sub},
};

use rust::Position;

static FILE_PATH: &'static str = "../data/day8.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 8");
    part1(&contents);
    part2(&contents);
}

type Bounds = (usize, usize);

#[derive(Clone, Copy, Debug)]
struct Vector(f32, f32);

impl Vector {
    fn scale(&self, scalar: f32) -> Self {
        let Vector(x, y) = self;
        Vector(scalar * x, scalar * y)
    }

    fn from_position(pos: &Position) -> Self {
        Vector(pos.x as f32, pos.y as f32)
    }

    fn to_position(&self) -> Option<Position> {
        let Vector(x, y) = self;
        let x = x.round();
        let y = y.round();
        if x >= 0.0 && y >= 0.0 {
            Some(Position {
                x: x as usize,
                y: y as usize,
            })
        } else {
            None
        }
    }
}

impl Add for Vector {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let Vector(x0, y0) = self;
        let Vector(x1, y1) = rhs;
        Vector(x0 + x1, y0 + y1)
    }
}

impl Sub for Vector {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + rhs.scale(-1.0)
    }
}

struct DataReadResult {
    map: HashMap<char, Vec<Position>>,
    bounds: Bounds,
}

fn read_data(contents: &str) -> DataReadResult {
    let mut map: HashMap<char, Vec<Position>> = HashMap::new();
    contents.lines().enumerate().for_each(|(y, line)| {
        line.chars().enumerate().for_each(|(x, val)| {
            let position = Position { x, y };
            if val != '.' {
                map.entry(val)
                    .and_modify(|array| array.push(position))
                    .or_insert(vec![position]);
            }
        })
    });

    let first = contents.lines().next().unwrap();
    let bounds = (first.len(), contents.lines().count());

    DataReadResult { map, bounds }
}

fn position_within_bounds(pos: &Position, bounds: &Bounds) -> bool {
    pos.x < bounds.0 && pos.y < bounds.1
}

fn find_anti_nodes(left: &Position, right: &Position, bounds: &Bounds) -> Vec<Position> {
    let left_v = Vector::from_position(left);
    let right_v = Vector::from_position(right);
    let v = right_v - left_v;

    let left_antinode = left_v - v;
    let right_antinode = right_v + v;

    [left_antinode, right_antinode]
        .iter()
        .filter_map(|v| v.to_position())
        .filter(|p| position_within_bounds(&p, bounds))
        .collect::<Vec<Position>>()
}

fn find_more_anti_nodes(left: &Position, right: &Position, bounds: &Bounds) -> Vec<Position> {
    let left_v = Vector::from_position(left);
    let right_v = Vector::from_position(right);
    let slope = right_v - left_v;

    let mut nodes = Vec::new();
    let mut current_vec = left_v;
    loop {
        let new_vec = current_vec + slope;
        if let Some(pos) = new_vec.to_position() {
            if position_within_bounds(&pos, bounds) {
                nodes.push(pos);
                current_vec = new_vec;
                continue;
            }
        }
        break;
    }
    current_vec = left_v;
    loop {
        let new_vec = current_vec - slope;
        if let Some(pos) = new_vec.to_position() {
            if position_within_bounds(&pos, bounds) {
                nodes.push(pos);
                current_vec = new_vec;
                continue;
            }
        }
        break;
    }

    nodes
}

fn part1(contents: &str) -> usize {
    let data = read_data(contents);
    let collections = data.map.into_values();
    let mut all = Vec::new();
    for col in collections {
        for pos_1 in &col {
            for pos_2 in &col {
                if pos_1 != pos_2 {
                    all.extend(find_anti_nodes(pos_1, pos_2, &data.bounds))
                }
            }
        }
    }
    all.sort();
    all.dedup();
    let answer = all.len();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let data = read_data(contents);
    let collections = data.map.into_values();
    let mut all = Vec::new();
    for col in collections {
        for pos_1 in &col {
            for pos_2 in &col {
                if pos_1 != pos_2 {
                    all.extend(find_more_anti_nodes(pos_1, pos_2, &data.bounds))
                }
            }
        }
    }
    all.sort();
    all.dedup();
    let answer = all.len();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 14);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 34);
    }
}

