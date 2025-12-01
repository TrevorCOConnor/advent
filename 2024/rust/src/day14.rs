use regex::Regex;
use rust::{Matrix, Position};

static FILE_PATH: &'static str = "../data/day14.txt";
// static WIDTH: i32 = 101;
// static LENGTH: i32 = 103;
static WIDTH: i32 = 11;
static LENGTH: i32 = 7;

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 14");
    part1(&contents);
    part2(&contents);
}

#[derive(Debug)]
struct RobotStats {
    position: (i32, i32),
    velocity: (i32, i32),
}

impl RobotStats {
    fn time_jump(&self, seconds: i32) -> (i32, i32) {
        let (x, y) = self.position;
        let x_jump = (self.velocity.0 * (seconds % WIDTH)) % WIDTH;
        let y_jump = (self.velocity.1 * (seconds % LENGTH)) % LENGTH;

        let y_jump = {
            if y_jump.is_negative() {
                LENGTH + y_jump
            } else {
                y_jump
            }
        };

        let x_jump = {
            if x_jump.is_negative() {
                WIDTH + x_jump
            } else {
                x_jump
            }
        };

        ((x + x_jump) % WIDTH, (y + y_jump) % LENGTH)
    }
}

fn read_data(contents: &str) -> Vec<RobotStats> {
    let re = Regex::new(r"p=(?<p1>\d+),(?<p2>\d+) v=(?<v1>\-*\d+),(?<v2>\-*\d+)").unwrap();
    re.captures_iter(contents)
        .map(|mtch| RobotStats {
            position: (mtch["p1"].parse().unwrap(), mtch["p2"].parse().unwrap()),
            velocity: (mtch["v1"].parse().unwrap(), mtch["v2"].parse().unwrap()),
        })
        .collect()
}

fn calculate_quadrants(positions: &[(i32, i32)]) -> usize {
    let mut q1 = 0;
    let mut q2 = 0;
    let mut q3 = 0;
    let mut q4 = 0;

    for (x, y) in positions {
        let mut left = true;
        let mut bottom = true;

        if *x == (WIDTH - 1).div_euclid(2) || *y == (LENGTH - 1).div_euclid(2) {
            continue;
        }

        if *x > (WIDTH - 1).div_euclid(2) {
            left = false
        }

        if *y > (LENGTH - 1).div_euclid(2) {
            bottom = false;
        }

        if left && bottom {
            q3 += 1;
        } else if left && !bottom {
            q2 += 1;
        } else if !left && bottom {
            q4 += 1;
        } else {
            q1 += 1;
        }
    }

    q1 * q2 * q3 * q4
}

fn calculate_tree() -> Vec<(i32, i32)> {
    let mut positions = Vec::new();
    let mid = (WIDTH - 1).div_euclid(2);

    positions.push((mid, 0));
    for i in 1..(LENGTH - 1) {
        positions.push((mid - i, i));
        positions.push((mid + i, i));
    }
    positions
}

struct Mod(i32, i32);

impl Mod {
    fn new(val: i32, modulo: i32) -> Self {
        let modded = val % modulo;
        if modded.is_negative() {
            Mod(modulo + modded, modulo)
        } else {
            Mod(modded, modulo)
        }
    }

    fn mul_inv(&self) -> i32 {
        self.0.pow((self.1 - 2) as u32) % self.1
    }

    fn add_inv(&self) -> i32 {
        self.1 - self.0
    }
}

fn part1(contents: &str) -> usize {
    let stats = read_data(contents);
    println!("{}", stats.len());
    let positions = stats
        .iter()
        .map(|rs| rs.time_jump(100))
        .collect::<Vec<(i32, i32)>>();
    let answer = calculate_quadrants(&positions);
    println!("Part 1: {}", answer);
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

    static DATA: &'static str = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3";

    // #[test]
    // Gotta change the static vars for this to work
    // 11 wide
    // 7 tall
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 12);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 0);
    }
}
