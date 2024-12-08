use std::collections::{HashMap, HashSet};

use rust::{Direction, Matrix, Position};

static FILE_PATH: &'static str = "../data/day6.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 6");
    part1(&contents);
    part2(&contents);
}

#[derive(Clone)]
enum State {
    Blocked,
    Traversed,
    Untraversed,
}

impl State {
    fn is_blocked(&self) -> bool {
        match self {
            State::Blocked => true,
            _ => false,
        }
    }

    fn has_been_explored(&self) -> bool {
        match self {
            State::Traversed => true,
            _ => false,
        }
    }
}

fn read_data(contents: &str) -> Matrix<State> {
    let rows = contents
        .lines()
        .rev()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '#' => State::Blocked,
                    '.' => State::Untraversed,
                    _ => State::Traversed,
                })
                .collect::<Vec<State>>()
        })
        .collect::<Vec<Vec<State>>>();

    Matrix::new(rows)
}

fn find_starting_pos(matrix: &Matrix<State>) -> Position {
    matrix
        .values()
        .iter()
        .find(|(_, v)| v.has_been_explored())
        .unwrap()
        .0
}

fn traverse(matrix: &mut Matrix<State>, pos: &Position, dir: &Direction) -> Option<()> {
    let next_pos = pos.apply_dir(dir)?;
    let state = matrix.get(&next_pos)?;
    if state.is_blocked() {
        let new_dir = dir.rotate_90();
        traverse(matrix, &pos, &new_dir)
    } else {
        if !state.has_been_explored() {
            matrix.set(&next_pos, State::Traversed);
        }
        traverse(matrix, &next_pos, dir)
    }
}

struct LoopExplorer {
    loop_positions: HashSet<Position>,
    traverse_map: HashMap<Position, HashSet<Direction>>,
}

impl LoopExplorer {
    fn find_loops(matrix: &Matrix<State>, pos: &Position, dir: &Direction) -> HashSet<Position> {
        let mut loop_explorer = LoopExplorer {
            loop_positions: HashSet::new(),
            traverse_map: HashMap::new(),
        };
        let mut matrix_copy = matrix.clone();
        loop_explorer.explore(&mut matrix_copy, pos, dir);
        loop_explorer.loop_positions
    }

    fn explore(
        &mut self,
        matrix: &mut Matrix<State>,
        pos: &Position,
        dir: &Direction,
    ) -> Option<()> {
        let next_pos = pos.apply_dir(dir)?;
        let state = matrix.get(&next_pos)?;
        if state.is_blocked() {
            let new_dir = dir.rotate_90();
            self.explore(matrix, &pos, &new_dir)
        } else {
            if !self.traverse_map.contains_key(&next_pos) {
                if LoopChecker::check_branch(&self.traverse_map, matrix, &pos, dir) {
                    self.loop_positions.insert(next_pos);
                }
                self.traverse_map.insert(next_pos, HashSet::new());
            }
            self.traverse_map.get_mut(&next_pos).unwrap().insert(*dir);

            self.explore(matrix, &next_pos, dir)
        }
    }
}

struct LoopChecker {
    traverse_map: HashMap<Position, HashSet<Direction>>,
}

impl LoopChecker {
    fn check(&mut self, matrix: &Matrix<State>, pos: &Position, dir: &Direction) -> Option<()> {
        let next_pos = pos.apply_dir(dir)?;
        let state = matrix.get(&next_pos)?;
        if state.is_blocked() {
            let new_dir = dir.rotate_90();
            self.check(matrix, &pos, &new_dir)
        } else {
            if self
                .traverse_map
                .get(&next_pos)
                .is_some_and(|s| s.contains(&dir))
            {
                Some(())
            } else {
                if !self.traverse_map.contains_key(&next_pos) {
                    self.traverse_map.insert(next_pos, HashSet::new());
                }
                self.traverse_map.get_mut(&next_pos).unwrap().insert(*dir);
                self.check(matrix, &next_pos, dir)
            }
        }
    }

    fn check_branch(
        traverse_map: &HashMap<Position, HashSet<Direction>>,
        matrix: &mut Matrix<State>,
        pos: &Position,
        dir: &Direction,
    ) -> bool {
        let mut loop_checker = LoopChecker {
            traverse_map: traverse_map.clone(),
        };
        loop_checker.traverse_map = traverse_map.clone();
        let next_pos = pos.apply_dir(dir).unwrap();
        matrix.set(&next_pos, State::Blocked);
        let res = loop_checker.check(matrix, pos, dir);
        matrix.set(&next_pos, State::Untraversed);
        res.is_some()
    }
}

fn part1(contents: &str) -> usize {
    let mut matrix = read_data(contents);
    let starting_pos = find_starting_pos(&matrix);
    let starting_dir = Direction::North;
    traverse(&mut matrix, &starting_pos, &starting_dir);
    let answer = matrix
        .values()
        .iter()
        .filter(|(_, v)| v.has_been_explored())
        .count();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let matrix = read_data(contents);
    let starting_pos = find_starting_pos(&matrix);
    let starting_dir = Direction::North;

    let answer = LoopExplorer::find_loops(&matrix, &starting_pos, &starting_dir).len();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 41);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 6);
    }
}
