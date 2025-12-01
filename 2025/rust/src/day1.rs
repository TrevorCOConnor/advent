const FILE_PATH: &'static str = "../data/day1.txt";
const START: i32 = 50;
const MOD: i32 = 100;

fn parse_input(inp: &str) -> Vec<i32> {
    inp.lines()
        .map(|l| {
            if l.starts_with("L") {
                -l[1..].parse::<i32>().expect("Bad input!")
            } else {
                l[1..].parse::<i32>().expect("Bad input!")
            }
        })
        .collect()
}

fn part1(file_contents: &str) -> u32 {
    let instructions = parse_input(file_contents);
    let mut cur = START;
    let mut zeroes = 0;
    for instr in instructions {
        cur += instr;
        if cur % MOD == 0 {
            zeroes += 1;
        }
    }

    zeroes
}

fn part2(file_contents: &str) -> u32 {
    let instructions = parse_input(file_contents);
    let mut cur = START;
    let mut zeroes = 0;
    for instr in instructions {
        let prev = cur.signum();

        cur += instr;
        zeroes += cur.abs().div_euclid(MOD) as u32;

        if prev != 0 && cur.signum() != prev {
            zeroes += 1;
        }
        cur %= MOD;
    }

    zeroes
}

pub fn solution() -> Result<(), Box<dyn std::error::Error>> {
    let contents = std::fs::read_to_string(FILE_PATH)?;
    println!("Day 1");
    let p1 = part1(&contents);
    println!("\tPart 1: {}", p1);
    let p2 = part2(&contents);
    println!("\tPart 2: {}", p2);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::day1::{part1, part2};

    const DATA: &'static str = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

    #[test]
    fn test_sample_part1() {
        let res = part1(DATA);
        assert_eq!(res, 3)
    }

    #[test]
    fn test_sample_part2() {
        let res = part2(DATA);
        assert_eq!(res, 6)
    }
}
