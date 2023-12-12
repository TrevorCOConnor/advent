use std::str::FromStr;

static FILE_PATH: &'static str = "../data/day11.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken file.");

    println!("Day 11");
    part1(&contents);
    part2(&contents, 1000000);
}

#[derive(Debug)]
struct Position {
    x: usize,
    y: usize
}

impl Position {
    fn manhattan_distance(&self, other: &Position) -> usize {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }

    fn expand(
        &self,
        expansion_size: usize,
        cols: &[usize],
        rows: &[usize]
    ) -> Position {
        let expansion_size = expansion_size - 1;

        let col_expansion = cols.into_iter()
            .filter(|col| **col < self.x)
            .count();

        let row_expansion = rows.into_iter()
            .filter(|row| **row < self.y)
            .count();
        
        Position {
            x: self.x + col_expansion * expansion_size,
            y: self.y + row_expansion * expansion_size
        }
    }
}

struct Space(Vec<Vec<Option<()>>>);

impl FromStr for Space {
    // Skip errors
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        Ok(
            Space(
                s
                    .lines()
                    .filter(|l| !l.is_empty())
                    .map(|l| l.chars().map(|c|
                            if c == '#' { Some(()) } else { None }
                        ).collect()
                    )
                    .collect()
            )
        )
    }
}

impl Space {
    fn get_galaxy_locations(&self) -> Vec<Position> {
        self.0
            .iter()
            .enumerate()
            .map(|(row_idx, row)| row
                .iter()
                .enumerate()
                .filter(|(_, value)| value.is_some())
                .map(|(col_idx, _)| Position { x: col_idx, y: row_idx })
                .collect::<Vec<Position>>()
            )
            .flatten()
            .collect()
    }

    fn get_empty_rows(&self) -> Vec<usize> {
         self.0
            .iter()
            .enumerate()
            .filter(|(_, vec)| vec.iter().all(|value| value.is_none()))
            .map(|(row_idx, _)| row_idx)
            .collect()
    }
    
    fn get_empty_cols(&self) -> Vec<usize> {
        (0..self.0.first().unwrap().len())
            .into_iter()
            .filter(|col_idx| self.0.iter().all(|vec| vec[*col_idx].is_none()))
            .collect::<Vec<usize>>()
    }

    fn get_expanded_galaxy_locations(
        &self, expansion_size: usize
    ) -> Vec<Position> {
        let galaxy_locations = self.get_galaxy_locations();
        let empty_cols = self.get_empty_cols();
        let empty_rows = self.get_empty_rows();

        galaxy_locations.into_iter()
            .map(|pos| pos.expand(expansion_size, &empty_cols, &empty_rows))
            .collect()
    }

    fn calculate_lengths_between_galaxies(
        &self, expansion_size: usize
    ) -> usize {
        let galaxies = self.get_expanded_galaxy_locations(expansion_size);
        galaxies
            .iter()
            .enumerate()
            .map(|(idx, left)| galaxies[idx..]
                .iter()
                .map(|right| left.manhattan_distance(right))
                .sum::<usize>()
            ).sum()
    }
}

fn part1(contents: &str) -> usize {
    let answer = contents
        .parse::<Space>()
        .unwrap()
        .calculate_lengths_between_galaxies(1);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str, expansion_size: usize) -> usize {
    let answer = contents
        .parse::<Space>()
        .unwrap()
        .calculate_lengths_between_galaxies(expansion_size);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 374);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA, 10);
        assert_eq!(answer, 1030);
    }
}
