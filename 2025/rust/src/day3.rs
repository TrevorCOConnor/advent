const FILE_PATH: &'static str = "../data/day3.txt";

fn parse_input(inp: &str) -> Vec<Vec<u64>> {
    inp.lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.chars()
                .filter_map(|c| c.to_digit(10).map(|v| v as u64))
                .collect()
        })
        .collect()
}

fn find_max_voltage(block: &[u64], num_batteries: usize) -> u64 {
    let voltage = block.iter().enumerate().collect::<Vec<(usize, &u64)>>();
    let voltage_len = voltage.len();

    let mut batteries: Vec<u64> = Vec::new();
    let mut left_offset = 0;
    for i in 0..num_batteries {
        let right_offset = voltage_len - (num_batteries - i);
        let max = voltage[left_offset..=right_offset]
            .iter()
            // had to use location in max calc because it does not always default to the first
            // instance
            .max_by_key(|v| (v.1, -1 * (v.0 as i32)))
            .expect("blocks should not be empty");

        left_offset = max.0 + 1;
        batteries.push(*max.1);
    }

    let res = batteries
        .iter()
        .rev()
        .enumerate()
        .map(|(p, v)| v * 10u64.pow(p as u32))
        .sum();
    res
}

fn part1(file_contents: &str) -> u64 {
    let blocks = parse_input(file_contents);
    blocks.iter().map(|b| find_max_voltage(b, 2)).sum()
}

fn part2(file_contents: &str) -> u64 {
    let blocks = parse_input(file_contents);
    blocks.iter().map(|b| find_max_voltage(b, 12)).sum()
}

pub fn solution() -> Result<(), Box<dyn std::error::Error>> {
    let contents = std::fs::read_to_string(FILE_PATH)?;
    println!("Day 3");
    let p1 = part1(&contents);
    println!("\tPart 1: {}", p1);
    let p2 = part2(&contents);
    println!("\tPart 2: {}", p2);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::day3::{part1, part2};

    const DATA: &'static str = "987654321111111
811111111111119
234234234234278
818181911112111";

    #[test]
    fn test_sample_part1() {
        let res = part1(DATA);
        assert_eq!(res, 357)
    }

    #[test]
    fn test_sample_part2() {
        let res = part2(DATA);
        assert_eq!(res, 3121910778619)
    }
}
