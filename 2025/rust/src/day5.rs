use std::collections::VecDeque;

const FILE_PATH: &'static str = "../data/day5.txt";

type Range = (u64, u64);

fn parse_input(inp: &str) -> (Vec<Range>, Vec<u64>) {
    let ranges: Vec<Range> = inp
        .lines()
        .take_while(|l| !l.is_empty())
        .filter_map(|l| {
            let (x, y) = l.trim().split_once("-")?;
            Some((x.parse::<u64>().ok()?, y.parse::<u64>().ok()?))
        })
        .collect();

    let ids: Vec<u64> = inp
        .lines()
        .skip_while(|l| !l.is_empty())
        .filter_map(|l| Some(l.trim().parse::<u64>().ok()?))
        .collect();

    (ranges, ids)
}

fn fresh_ids(ranges: &[Range], ids: &[u64]) -> Vec<u64> {
    let mut rng = ranges.to_vec();
    rng.sort();

    let mut ids = ids.to_vec();
    ids.sort();

    let mut start_queue = VecDeque::from_iter(rng.iter().map(|v| v.0));
    let mut end_queue = VecDeque::from_iter(rng.iter().map(|v| v.1));

    let mut start = start_queue.pop_front();
    let mut end = end_queue.pop_front();
    let mut fresh = Vec::new();

    for i in ids {
        loop {
            if start.map(|v| v > i).unwrap_or(true) {
                break;
            }

            if end.map(|v| v < i).unwrap_or(true) {
                start = start_queue.pop_front();
                end = end_queue.pop_front();
                continue;
            }

            fresh.push(i);
            break;
        }
    }

    fresh
}

fn compress_ranges(ranges: &[Range]) -> u64 {
    let mut ranges = Vec::from_iter(ranges);
    ranges.sort();

    let mut range_queue = VecDeque::from_iter(ranges);
    let mut compressed_ranges = Vec::new();

    let mut current = *range_queue.pop_front().unwrap();
    for rng in range_queue {
        if rng.0 > current.1 {
            compressed_ranges.push(current);
            current = *rng;
        }

        if current.0 <= rng.0 && rng.0 <= current.1 {
            current.1 = rng.1.max(current.1);
        }
    }
    compressed_ranges.push(current);

    compressed_ranges.iter().map(|(a, b)| b - a + 1).sum()
}

fn part1(file_contents: &str) -> usize {
    let (ranges, ids) = parse_input(file_contents);
    fresh_ids(&ranges, &ids).len()
}

fn part2(file_contents: &str) -> u64 {
    let (ranges, _) = parse_input(file_contents);
    compress_ranges(&ranges)
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
    use crate::day5::{part1, part2};

    const DATA: &'static str = "3-5
10-14
16-20
12-18

1
5
8
11
17
32";

    #[test]
    fn test_sample_part1() {
        let res = part1(DATA);
        assert_eq!(res, 3)
    }

    #[test]
    fn test_sample_part2() {
        let res = part2(DATA);
        assert_eq!(res, 14)
    }
}
