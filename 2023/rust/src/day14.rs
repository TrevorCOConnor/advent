// This one is for sure salvagable, but I was very tired

use std::{str::FromStr, collections::HashMap};

use itertools::Itertools;
use num::Integer;

static FILE_PATH: &'static str = "../data/day14.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 14");
    part1(&contents);
    part2(&contents, 1_000_000_000);
}

enum Direction {
    North,
    East,
    South,
    West
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
enum Rock { Square, Circle }

impl Rock {
    fn from_char(c: char) -> Option<Rock> {
        match c {
            'O' => Some(Rock::Circle),
            '#' => Some(Rock::Square),
            _ => None
        }
    }

    fn is_square(&self) -> bool {
        match self {
            Rock::Square => true,
            _ => false
        }
    }

    fn is_circle(&self) -> bool {
        !self.is_square()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct Dish {
    matrix: Vec<Vec<Option<Rock>>>,
    width: usize,
}

impl FromStr for Dish {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let contents = s.lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                line.chars()
                    .map(Rock::from_char)
                    .collect::<Vec<Option<Rock>>>()
            })
            .collect::<Vec<Vec<Option<Rock>>>>();
       let width = contents.first().map(|v| v.len()).unwrap_or(0);

       Ok(Dish { matrix: contents, width })
    }
}

impl Dish {
    fn get(&self, col: usize, row: usize) -> &Option<Rock> {
        &self.matrix[row][col]
    }

    fn get_col(&self, col: usize) -> Vec<&Option<Rock>> {
        (0..self.matrix.len())
            .into_iter()
            .map(|row| {
                self.get(col, row)
            })
            .collect()
    }

    fn tilt_slice(slice: &[&Option<Rock>], close: bool) -> Vec<Option<Rock>> {
        slice.split(|r| r.is_some_and(|v| v.is_square()))
            .map(|piece| {
                let circles = piece.iter().filter(|v| v.is_some()).count();
                if close {
                    std::iter::repeat(Some(Rock::Circle)).take(circles)
                        .chain(std::iter::repeat(None).take(piece.len() - circles))
                        .collect::<Vec<Option<Rock>>>()
                } else {
                    std::iter::repeat(None).take(piece.len() - circles)
                        .chain(
                            std::iter::repeat(Some(Rock::Circle)).take(circles)
                        )
                        .collect::<Vec<Option<Rock>>>()
                }
            })
            .reduce(|mut acc, new| {
                acc.push(Some(Rock::Square));
                acc.extend(new);
                acc
            })
            .unwrap()
    }

    fn collect_columns(columns: Vec<Vec<Option<Rock>>>) -> Dish {
        let matrix_length = columns.first().unwrap().len();
        let matrix_width = columns.len();

        let mut matrix: Vec<Vec<Option<Rock>>> = vec![Vec::new(); matrix_length];
        for col in 0..matrix_width {
            for row in 0..matrix_length {
                matrix[row]
                    .push(columns[col][row])
            }
        }

        Dish { matrix, width: matrix_width }
    }

    fn collect_rows(rows: Vec<Vec<Option<Rock>>>) -> Dish {
        let width = rows.first().unwrap().len();
        Dish { matrix: rows.to_vec(), width }
    }

    fn evaluate_column(col: &[&Option<Rock>]) -> usize {
        col
        .iter()
        .rev()
        .enumerate()
        .map(|(loc, maybe_rock)| {
            if maybe_rock.is_some_and(|rock| rock.is_circle()) {
                loc + 1
            } else {
                0
            }
        })
        .sum()
    }

    fn calculate_load(&self) -> usize {
        (0..self.width)
            .into_iter()
            .map(|idx| {
                Self::evaluate_column(
                    &self.get_col(idx)
                )
            })
            .sum()
    }

    fn tilt(self, direction: Direction) -> Dish {
        match direction {
            Direction::North => { 
                Dish::collect_columns(
                    (0..self.width)
                        .into_iter()
                        .map(|idx| {
                            Self::tilt_slice(
                                &self.get_col(idx),
                                true
                            )
                        })
                        .collect_vec()
                )
            },
            Direction::West => {
                Dish::collect_rows(
                    self.matrix
                        .into_iter()
                        .map(|row| Self::tilt_slice(
                            &row.iter().collect_vec(), true
                        ))
                        .collect()
                )
            },
            Direction::South => {
                Dish::collect_columns(
                    (0..self.width)
                        .into_iter()
                        .map(|idx| {
                            Self::tilt_slice(
                                &self.get_col(idx),
                                false
                            )
                        })
                        .collect_vec()
                )
            },
            Direction::East => {
                Dish::collect_rows(
                    self.matrix
                        .into_iter()
                        .map(|row| Self::tilt_slice(
                            &row.iter().collect_vec(), false
                        ))
                        .collect()
                )
            }
        }
    }

    fn cycle(self) -> Dish {
        self.tilt(Direction::North)
            .tilt(Direction::West)
            .tilt(Direction::South)
            .tilt(Direction::East)
    }

    fn cycle_n(self, n: u32) -> Dish {
        let mut dish_cache: HashMap<Dish, u32> = HashMap::new();
        let mut initial_step: u32 = 0;
        let mut repeat_step: u32 = 0;

        let dish = (0..n)
            .fold_while(self, |acc, step| {
                dish_cache.insert(acc.clone(), step);
                let new = acc.cycle();
                if dish_cache.contains_key(&new) {
                    repeat_step = 1 + (step - dish_cache[&new]);
                    initial_step = step;
                    itertools::FoldWhile::Done(new)
                } else {
                    itertools::FoldWhile::Continue(new)
                }
            })
            .into_inner();

        if repeat_step != 0 {
            let new_n = (n - (initial_step + 1)).div_rem(&repeat_step).1;
            dish.cycle_n(new_n)
        } else {
            dish
        }
    }
}


fn part1(contents: &str) -> usize {
    let dish = contents.parse::<Dish>().unwrap();
    let answer = dish.tilt(Direction::North).calculate_load();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str, cycle_count: u32) -> usize {
    let dish = contents.parse::<Dish>().unwrap();
    let answer = dish
        .cycle_n(cycle_count)
        .calculate_load();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 136);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA, 1_000_000_000);
        assert_eq!(answer, 64);
    }

}
    
