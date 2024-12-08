static FILE_PATH: &'static str = "../data/day4.txt";

use std::ascii::AsciiExt;

use rust::{Direction, Matrix, Position};

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 4");
    part1(&contents);
    part2(&contents);
}

fn read_data(contents: &str) -> Matrix<char> {
    Matrix::new(
        contents.lines()
            .filter(|l| !l.is_empty())
            .map(|l| l.chars().collect())
            .rev()
            .collect()
    )
}

fn check_dir(matrix: &Matrix<char>, pos: &Position, dir: &Direction) -> bool {
    let mut current_pos = pos.clone();
    for check_val in ['M', 'A', 'S'] {
        let next_pos = current_pos.apply_dir(dir);
        if next_pos.is_none() {
            return false;
        }
        current_pos = next_pos.unwrap();
        if let Some(val) = matrix.get(&current_pos) {
            if val.to_ascii_uppercase() != check_val {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

fn check_x(matrix: &Matrix<char>, pos: &Position) -> bool {
    let mut left_diagonal = (false, false);
    let mut right_diagonal = (false, false);
    for (diagonal, dirs) in [
        (&mut left_diagonal, [Direction::NorthWest, Direction::SouthEast]),
        (&mut right_diagonal, [Direction::NorthEast, Direction::SouthWest])
    ] {
        for dir in dirs {
            let new_pos = pos.apply_dir(&dir);
            if new_pos.is_none() {
                return false;
            }
            let val = matrix.get(&new_pos.unwrap());
            if val.is_none() {
                return false;
            }
            let val = val.unwrap();
            if val.to_ascii_uppercase() == 'M' {
                diagonal.0 = true;
            } else if val.to_ascii_uppercase() == 'S' {
                diagonal.1 = true;
            }
        }
    }

    left_diagonal.0 & left_diagonal.1 & right_diagonal.0 & right_diagonal.1
}

fn find_all_xmas(matrix: &Matrix<char>) -> u32 {
    let mut count: u32 = 0;
    for (pos, val) in matrix.values() {
        if val.to_ascii_uppercase() == 'X' {
            for dir in Direction::all() {
                if check_dir(matrix, &pos, &dir) {
                    count += 1
                }
            }
        }
    }
    count
}

fn find_all_x_mas(matrix: &Matrix<char>) -> u32 {
    let mut count: u32 = 0;
    for (pos, val) in matrix.values() {
        if val.to_ascii_uppercase() == 'A' {
            if check_x(matrix, &pos) {
                count += 1;
            }
        }
    }
    count
}

fn part1(contents: &str) -> u32 {
    let matrix = read_data(contents);
    let answer = find_all_xmas(&matrix);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let matrix = read_data(contents);
    let answer = find_all_x_mas(&matrix);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 18);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 9);
    }
}
