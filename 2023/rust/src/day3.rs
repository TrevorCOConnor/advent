// Dear reader,
// I was experimenting with some things in this problem
// and as a result the code got unruely. I will clean it up later

use std::ops::Range;
use bitvec::prelude::*;
use regex::Regex;

static FILE_PATH: &'static str = "../data/day3.txt";

fn buffer_range(index: &usize, max: Option<&usize>) -> Range<usize> {
    let end = {
        if let Some(max) = max {
            (index + 1).min(max - 1)
        } else {
            index + 1
        }
    };

    Range { start: index.max(&1) - 1, end: end + 1 }
}

struct Location(BitVec);

impl Location {
    fn from_range(range: Range<usize>) -> Location {
        let mut bmap = bitvec![0; 256];
        for i in range {
            bmap.set(i, true);
        }
        Location(bmap)
    }

    fn intersects(&self, other: &Location) -> bool {
        // Hate this
        (self.0.clone() & other.0.clone()).any()
    }

}

struct Schematic<T> {
    value: T,
    location: Location
}

impl Schematic<u32> {
    fn from_line(line: &str) -> Vec<Schematic<u32>> {
        let re = Regex::new(r"\d+").unwrap();
        re.find_iter(line)
            .map(|m| Schematic {
                    value: m.as_str().parse().unwrap(),
                    location: Location::from_range(m.range())
                }
            )
            .collect()
    }
}

struct SymbolMatrix(Vec<BitVec>);

impl SymbolMatrix {
    fn all_symbols(text: &str) -> SymbolMatrix {
        let lines: Vec<&str> = text.lines()
            .filter(|l| !l.is_empty())
            .collect();

        let length = lines.len();
        let mut matrix = vec![bitvec![0; 256]; length];

        let re = Regex::new(r"[^\d\.]").unwrap();
        for (row, line) in lines.iter().enumerate() {
            // Find symbol indices
            let indices: Vec<usize> = re.find_iter(line)
                .map(|m| m.start())
                .collect();

            // Mark true for surrounding areas
            for buffered_row in buffer_range(&row, Some(&length)) {
                for index in indices.iter() {
                    for buffered_index in buffer_range(index, None) {
                        matrix[buffered_row].set(buffered_index, true)
                    }
                }
            }
        }

        SymbolMatrix(matrix)
    }

    fn gears_ratio(text: &str, numbers: &[Vec<Schematic<u32>>]) -> u32 {
        let lines: Vec<&str> = text.lines()
            .filter(|l| !l.is_empty())
            .collect();

        let re = Regex::new(r"\*").unwrap();
        let mut ratios: u32 = 0;
        for (row, line) in lines.iter().enumerate() {
            let new = re.find_iter(line)
                .map(
                    |m| {
                        let loc = Location::from_range(buffer_range(&m.start(), None));
                        let range = buffer_range(&row, Some(&numbers.len()));
                        let adjacent_numbers: Vec<&Schematic<u32>> = numbers[range.start..range.end]
                            .iter()
                            .map(|vec| vec.iter().filter(|n| n.location.intersects(&loc)))
                            .flatten()
                            .collect();
                        if adjacent_numbers.len() == 2 {
                            adjacent_numbers.iter().map(|v| v.value).product()
                        } else {0}
                    }
                )
                .sum::<u32>();
            ratios += new;
        };

        ratios
    }
}

pub fn solutions() {
    println!("Day 3");
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let symbol_matrix = SymbolMatrix::all_symbols(contents);
    let answer = contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| Schematic::<u32>::from_line(l))
        .zip(symbol_matrix.0)
        .map(
            |(sn, sm)| sn.iter().filter(
                |s| { (s.location.0.clone() & sm.clone()).any() }
            ).map(|s| s.value).sum::<u32>()
        ).sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let schematic_numbers: Vec<Vec<Schematic<u32>>> = contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| Schematic::<u32>::from_line(l))
        .collect();
    let answer = SymbolMatrix::gears_ratio(&contents, &schematic_numbers);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        let data = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";
        let result = part1(data);
        assert_eq!(result, 4361)
    }

    #[test]
    fn test_part2() {
        let data = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";
        let result = part2(data);
        assert_eq!(result, 467835)
    }
}
