use std::{str::FromStr, ops::BitOr};

use itertools::{Itertools, FoldWhile};

static FILE_PATH: &'static str = "../data/day13.txt";

#[derive(Eq, PartialEq, Debug)]
struct Row(Vec<bool>);

impl FromStr for Row {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        Ok(
            Row(
                line.chars()
                    .map(|c| match c {
                        '.' => false,
                        '#' => true,
                        _ => panic!("Unknown character {c}"),
                    })
                    .collect()
            )
        )
    }
}

impl Row {
    fn has_symmetry_at(&self, idx: usize) -> bool {
        let length = self.0.len();

        // bounds
        let lower_bound = {
            if 2*idx > length {2*idx - length} else {0}
        };
        let upper_bound = {
            if 2*idx > length {length} else {2*idx}
        };

        // arrays
        let left = &self.0[lower_bound..idx];
        let right = &self.0[idx..upper_bound];
        let result = left.iter().zip(right.iter().rev())
            .all(|(l, r)| l == r);
        result
    }

    fn mirror_at(&self, idx: usize) -> Mirror<usize> {
        let length = self.0.len();

        // bounds
        let lower_bound = {
            if 2*idx > length {2*idx - length} else {0}
        };
        let upper_bound = {
            if 2*idx > length {length} else {2*idx}
        };

        // arrays
        let left = &self.0[lower_bound..idx];
        let right = &self.0[idx..upper_bound];
        let discrepancies = left.iter().zip(right.iter().rev())
            .filter(|(l, r)| l != r)
            .count();
        Mirror::from_discrepancies(discrepancies, idx)
    }

    fn find_vertical_symmetries(&self) -> Vec<usize> {
        let mut indices = Vec::new();

        for idx in 1..(self.0.len()) {
            if self.has_symmetry_at(idx) {
                indices.push(idx)
            }
        }
        indices
    }

    fn mirror_find_vertical_symmetries(&self) -> Vec<Mirror<usize>> {
        let mut indices = Vec::new();

        for idx in 1..(self.0.len()) {
            let mirror = self.mirror_at(idx);

            if !mirror.is_no() {
                indices.push(mirror)
            }
        }
        indices
    }

    fn compare_with_smudge(&self, other: &Row) -> Mirror<()> {
        let discrepancies = self.0
            .iter()
            .zip(other.0.iter())
            .filter(|(l, r)| l != r)
            .count();
        Mirror::from_discrepancies(discrepancies, ())
    }
}

#[derive(Debug)]
struct Pattern(Vec<Row>);

impl FromStr for Pattern {
    // skip errors
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(
            Pattern(
                s.lines() 
                    .map(|l| l.parse::<Row>().unwrap())
                    .collect()
            )
        )
    }
}

impl Pattern {
    fn find_horizontal_symmetry(&self) -> Option<usize> {
        for idx in 1..self.0.len() {
            if self.0[idx-1] == self.0[idx] {
                let cap = idx.min(self.0.len() - idx);
                if (1..cap).all(|offset| {
                    self.0[idx - (1 + offset)] == self.0[idx + offset]
                }) {
                    return Some(idx)
                }
            }
        }
        None
    }

    fn smudge_find_horizontal_symmetry(&self) -> Option<usize> {
        for idx in 1..self.0.len() {
            let start = self.0[idx-1].compare_with_smudge(&self.0[idx]);
            if !start.is_no() {
                let cap = idx.min(self.0.len() - idx);
                if let Mirror::Smudge(_) = (1..cap)
                    .map(|offset| {
                        self.0[idx - (1 + offset)].compare_with_smudge(&self.0[idx + offset])
                    })
                    .fold(start, |acc, new| acc | new)
                {
                    return Some(idx)
                }
            }
        }
        None
    }

    fn find_vertical_symmetry(&self) -> Option<usize> {
        let first = self.0.iter()
            .next()
            .unwrap()
            .find_vertical_symmetries();

        self.0[1..].into_iter()
            .fold_while(first, |acc, row| {
                let new = acc
                    .into_iter()
                    .filter(|idx| row.has_symmetry_at(*idx))
                    .collect::<Vec<usize>>();
                if new.len() == 1 {
                    FoldWhile::Done(new)
                } else {
                    FoldWhile::Continue(new)
                }
            })
            .into_inner()
            .into_iter()
            .next()
    }

    fn smudge_find_vertical_symmetry(&self) -> Option<usize> {
        let first = self.0.iter()
            .next()
            .unwrap()
            .mirror_find_vertical_symmetries();

        self.0[1..].into_iter()
            .fold(first, |acc, row| {
                acc
                    .into_iter()
                    .map(|idx| row.mirror_at(*idx.unwrap()) | idx)
                    .filter(|idx| !idx.is_no())
                    .collect::<Vec<Mirror<usize>>>()
            })
            .into_iter()
            .find_map(|v| v.smudge())
    }

    fn find_symmetry(&self) -> usize {
        if let Some(horizontal_symmetry) = self.find_horizontal_symmetry() {
            horizontal_symmetry * 100
        } else {
            self.find_vertical_symmetry().unwrap()
        }
    }

    fn find_symmetry_with_smudge(&self) -> usize {
        if let Some(horizontal_symmetry) = self.smudge_find_horizontal_symmetry() {
            horizontal_symmetry * 100
        } else {
            self.smudge_find_vertical_symmetry().unwrap()
        }
    }
}

fn parse_input(text: &str) -> Vec<Pattern> {
    let mut patterns = Vec::new();
    let mut current_pattern = Vec::new();

    for line in text.lines().chain(["\n"]) {
        let line = line.trim();
        if line.is_empty() {
            if current_pattern.len() > 0 {
                patterns.push(
                    Pattern(
                        current_pattern.drain(..).collect()
                    )
                )
            }
        } else {
            current_pattern.push(line.parse::<Row>().unwrap())
        }
    }

    patterns
}

#[derive(Debug)]
enum Mirror<T> {
    Clean(T),
    Smudge(T),
    No
}

impl<T> BitOr for Mirror<T> {
    type Output = Mirror<T>;
    fn bitor(self, rhs: Self) -> Self::Output {
        match self {
            Mirror::No => Mirror::No,
            Mirror::Clean(_) => rhs,
            Mirror::Smudge(x) => {
                match rhs {
                    Mirror::No => Mirror::No,
                    Mirror::Clean(_) => Mirror::Smudge(x),
                    Mirror::Smudge(_) => Mirror::No
                }
            },
        } 
    }
}

impl<T> Mirror<T>
    where T: Copy
{
    fn smudge(&self) -> Option<T> {
        match self {
            Mirror::Smudge(x) => Some(*x),
            _ => None
        }
    }

    fn unwrap(&self) -> &T {
        match self {
            Mirror::Clean(x) => x,
            Mirror::Smudge(x) => x,
            _ => panic!("Dirty mirror")
        }
    }

    fn is_no(&self) -> bool {
        match self {
            Mirror::No => true,
            _ => false
        }
    }

    fn from_discrepancies(discrepancies: usize, value: T) -> Mirror<T> {
        match discrepancies {
            0 => Mirror::Clean(value),
            1 => Mirror::Smudge(value),
            _ => Mirror::No
        }
    }
}

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 13");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> usize {
    let answer = parse_input(contents)
        .iter()
        .map(|pattern| pattern.find_symmetry())
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let answer = parse_input(contents)
        .iter()
        .map(|pattern| pattern.find_symmetry_with_smudge())
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

    static EXTRA: &'static str = "
#.##.###.
..#.##.#.
.#......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 405);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 400);

        let extra = part2(EXTRA);
        assert_eq!(extra, 5);
    }

}
    
