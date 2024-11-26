use std::str::FromStr;

use rust::Direction;

static FILE_PATH: &'static str = "../data/day18.txt";


fn str_to_dir(s: &str) -> Result<Direction, String> {
    match s {
       "U" => Ok(Direction::Up),
       "D" => Ok(Direction::Down),
       "R" => Ok(Direction::Right), 
       "L" => Ok(Direction::Left),
       _ => Err(format!("Direction {} not recognized", s))
    }
}

struct Instruction {
    direction: Direction,
    steps: u32,
    color: String
}

impl Instruction {
    fn create_interval_from(&self, x: u32, y: u32) -> Interval {
        match self.direction {
            Direction::Up => Interval {
                x: Ray::Single(x),
                y: Ray::Range(y, y + self.steps)
            },
            Direction::Down => Interval {
                x: Ray::Single(x),
                y: Ray::Range(y - self.steps, y)
            },
            Direction::Right => Interval {
                x: Ray::Range(x, x + self.steps),
                y: Ray::Single(y)
            },
            Direction::Left => Interval {
                x: Ray::Range(x - self.steps, x),
                y: Ray::Single(y)
            }
        }
    }

    fn apply_to(&self, x: u32, y: u32) -> (u32, u32) {
        match self.direction {
            Direction::Up => (x, y + self.steps),
            Direction::Down => (x, y - self.steps),
            Direction::Right => (x + self.steps, y),
            Direction::Left => (x - self.steps, y)
        }
    }

    fn build_intervals_from_array(instructions: &[Instruction]) -> Vec<Interval> {
        let mut x = 0_u32;
        let mut y = 0_u32;

        let mut intervals = Vec::new();
        for instr in instructions {
            intervals.push(instr.create_interval_from(x, y));
            let new_loc = instr.apply_to(x, y);
            x = new_loc.0;
            y = new_loc.1;
        }

        intervals
    }
}


struct Interval {
    x: Ray,
    y: Ray
}

impl Interval {
    fn intersects(&self, y: u32) -> Option<&Ray> {
        if  self.y.contains(y) {
            Some(&self.x)
        } else {
            None
        }
    }
}


enum Ray {
    Range(u32, u32),
    Single(u32)
}

impl Ray {
    fn max_val(&self) -> u32 {
        match self {
           Ray::Single(a) => *a,
           Ray::Range(_, b) => *b
        }
    }

    fn contains(&self, val: u32) -> bool {
        match self {
           Ray::Single(a) => a == &val,
           Ray::Range(a, b) => a <= &val && &val <= b
        }
    }
}

impl Ord for Ray {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    }
}


impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let items = s.split(" ").collect::<Vec<&str>>();
        if items.len() != 3 {
            Err("Row does not match expected format".to_string())
        } else {
            let direction = str_to_dir(items[0])?;
            let steps = u32::from_str(items[1])
                .map_err(|e| e.to_string())?;
            let color = items[2].to_string();

            Ok(Self { direction, steps, color })
        }
    }
}

fn interior_measure_of_intervals(intervals: &[Interval]) -> u32 {
    let min_y = 0;
    let max_y = 0;
}

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 18");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> usize {
    let instructions = contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(|line| Instruction::from_str(line));

    let answer = 0;
    // println!("Part 1: {}", answer);
    println!("Part 1: TODO");
    answer
}

fn part2(contents: &str) -> usize {
    let answer = 0;
    // println!("Part 2: {}", answer);
    println!("Part 2: TODO");
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str =
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 0);
    }

    // #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 0);
    }

}
    
