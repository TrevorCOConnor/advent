use std::{
    cmp::min,
    collections::{HashMap, HashSet, VecDeque},
};

use priority_queue::PriorityQueue;
use rust::{Direction, Matrix, Position};

static FILE_PATH: &'static str = "../data/day16.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 16");
    part1(&contents);
    part2(&contents);
}

#[derive(Eq, PartialEq)]
enum Block {
    Start,
    Vacant,
    Wall,
    End,
}

fn read_data(contents: &str) -> Matrix<Block> {
    Matrix::new(
        contents
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        'S' => Block::Start,
                        '#' => Block::Wall,
                        'E' => Block::End,
                        _ => Block::Vacant,
                    })
                    .collect()
            })
            .collect(),
    )
}

fn min_traverse_matrix(matrix: &Matrix<Block>) -> isize {
    let starting_position = matrix
        .values()
        .iter()
        .find(|(_, b)| **b == Block::Start)
        .unwrap()
        .0;

    let mut priority_queue: PriorityQueue<(Position, Direction), isize> = PriorityQueue::new();
    priority_queue.push((starting_position, Direction::East), 0);

    loop {
        let ((pos, dir), priority) = priority_queue.pop().unwrap();
        // Step forward
        if let Some(forward) = pos.apply_dir(&dir) {
            if let Some(block) = matrix.get(&forward) {
                if *block == Block::End {
                    return -(priority - 1);
                } else if *block == Block::Vacant {
                    priority_queue.push((forward, dir), priority - 1);
                }
            }
        }

        // Rotate
        let rotate_right = dir.rotate_90();
        let rotate_left = dir.rotate_90().rotate_90().rotate_90();
        priority_queue.push((pos, rotate_right), priority - 1000);
        priority_queue.push((pos, rotate_left), priority - 1000);
    }
}

struct MinMapper {
    matrix: Matrix<Block>,
    start_map: HashMap<(Position, Direction), isize>,
    end_map: HashMap<(Position, Direction), isize>,
}

impl MinMapper {
    fn build(matrix: Matrix<Block>) -> Self {
        MinMapper {
            matrix: matrix,
            start_map: HashMap::new(),
            end_map: HashMap::new(),
        }
    }

    fn traverse_matrix(&mut self) {
        let starting_position = self
            .matrix
            .values()
            .iter()
            .find(|(_, b)| **b == Block::Start)
            .unwrap()
            .0;

        let mut priority_queue: PriorityQueue<(Position, Direction), isize> = PriorityQueue::new();
        priority_queue.push((starting_position, Direction::East), 0);

        loop {
            let ((pos, dir), priority) = priority_queue.pop().unwrap();
            self.start_map
                .entry((pos, dir))
                .and_modify(|v| *v = min(*v, priority.abs()))
                .or_insert(priority.abs());

            // Step forward
            if let Some(forward) = pos.apply_dir(&dir) {
                if let Some(block) = self.matrix.get(&forward) {
                    if *block == Block::End {
                        return;
                    } else if *block == Block::Vacant {
                        priority_queue.push((forward, dir), priority - 1);
                    }
                }
            }

            // Rotate
            let rotate_right = dir.rotate_90();
            let rotate_left = dir.rotate_90().rotate_90().rotate_90();
            priority_queue.push((pos, rotate_right), priority - 1000);
            priority_queue.push((pos, rotate_left), priority - 1000);
        }
    }

    fn traverse_matrix_reverse(&mut self) {
        let starting_position = self
            .matrix
            .values()
            .iter()
            .find(|(_, b)| **b == Block::End)
            .unwrap()
            .0;

        let mut priority_queue: PriorityQueue<(Position, Direction), isize> = PriorityQueue::new();
        priority_queue.push((starting_position, Direction::East), 0);
        priority_queue.push((starting_position, Direction::West), 0);
        priority_queue.push((starting_position, Direction::South), 0);
        priority_queue.push((starting_position, Direction::North), 0);

        loop {
            let ((pos, dir), priority) = priority_queue.pop().unwrap();
            self.end_map
                .entry((pos, dir))
                .and_modify(|v| *v = min(*v, priority.abs()))
                .or_insert(priority.abs());

            // Step forward
            if let Some(forward) = pos.apply_dir(&dir) {
                if let Some(block) = self.matrix.get(&forward) {
                    if *block == Block::Start {
                        return;
                    } else if *block == Block::Vacant {
                        priority_queue.push((forward, dir), priority - 1);
                    }
                }
            }

            // Rotate
            let rotate_right = dir.rotate_90();
            let rotate_left = dir.rotate_90().rotate_90().rotate_90();
            priority_queue.push((pos, rotate_right), priority - 1000);
            priority_queue.push((pos, rotate_left), priority - 1000);
        }
    }

    fn count_points(&self, min_score: isize) -> usize {
        let mut positions = self
            .start_map
            .iter()
            .filter(|((pos, dir), start_v)| {
                self.end_map
                    .get(&(*pos, dir.rotate_90().rotate_90()))
                    // .inspect(|end_v| println!("{} + {} = {}", start_v, end_v, **start_v + **end_v))
                    .map(|end_v| *start_v + end_v == min_score)
                    .is_some_and(|b| b)
            })
            .map(|((pos, _), _)| pos)
            .collect::<Vec<&Position>>();
        positions.sort();
        positions.dedup();
        positions.len() + 2 // Add start and end points
    }
}

fn part1(contents: &str) -> isize {
    let matrix = read_data(contents);
    let answer = min_traverse_matrix(&matrix);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let matrix = read_data(contents);
    let min_score = min_traverse_matrix(&matrix);
    let mut min_mapper = MinMapper::build(matrix);
    min_mapper.traverse_matrix();
    min_mapper.traverse_matrix_reverse();
    let answer = min_mapper.count_points(min_score);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 7036);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 45);
    }
}
