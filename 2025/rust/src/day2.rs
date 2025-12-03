const FILE_PATH: &'static str = "../data/day2.txt";

fn parse_input(inp: &str) -> Vec<(u64, u64)> {
    let mut ranges = Vec::new();
    for block in inp.split(",") {
        if block.is_empty() {
            continue;
        }

        let (min, max) = block.split_once("-").expect("not a range");
        ranges.push((
            min.trim().parse().expect("Min not an int!"),
            max.trim().parse().expect("Max not an int!"),
        ));
    }

    ranges
}

fn find_fake_ids(lower: u64, upper: u64, windows: bool) -> Vec<u64> {
    let mut ids = Vec::new();

    let lower_digits = lower.to_string().len();
    let upper_digits = upper.to_string().len();

    // Create subranges of equal lengths
    let mut sub_ranges: Vec<(u64, u64)> = Vec::new();
    for x in lower_digits..=upper_digits {
        let lower = lower.max(10u64.pow((x - 1) as u32));
        let upper = (10u64.pow(x as u32) - 1).min(upper);
        sub_ranges.push((lower, upper));
    }

    // evaluate subranges
    for (l, u) in sub_ranges {
        let length = l.to_string().len();
        let half_length = length.div_euclid(2);

        // Hack
        // If windows is false, we only check half length, and only if the original length is even
        // (in which case div_euclid and div_ceil are the same)
        let window_min = if windows {
            1
        } else {
            half_length.max(length.div_ceil(2))
        };

        // Evaluate windows
        for wdw in window_min..=half_length {
            if wdw == 0 {
                continue;
            }
            if length % wdw != 0 {
                continue;
            }
            // Generate windows
            let left = l.to_string()[0..wdw].parse::<u64>().unwrap();
            let right = u.to_string()[0..wdw].parse::<u64>().unwrap();

            // Create possible ids using windows and verify them
            for x in left..=right {
                let copies = length.div_euclid(wdw);
                let possible_id = (0..copies).map(|v| x * 10u64.pow((wdw * v) as u32)).sum();

                // We will get duplicates for things like 1111
                // which is generated from windows of length 1 and 2
                if ids.contains(&possible_id) {
                    continue;
                }
                if lower <= possible_id && possible_id <= upper {
                    ids.push(possible_id)
                }
            }
        }
    }

    ids
}

fn part1(file_contents: &str) -> u64 {
    let ranges = parse_input(file_contents);
    let mut total = 0;
    for (lwr, uppr) in ranges {
        let new: u64 = find_fake_ids(lwr, uppr, false).into_iter().sum();
        total += new;
    }

    total
}

fn part2(file_contents: &str) -> u64 {
    let ranges = parse_input(file_contents);
    let mut total = 0;
    for (lwr, uppr) in ranges {
        let new: u64 = find_fake_ids(lwr, uppr, true).into_iter().sum();
        total += new;
    }

    total
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
    use crate::day2::{part1, part2};

    const DATA: &'static str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    #[test]
    fn test_sample_part1() {
        let res = part1(DATA);
        assert_eq!(res, 1227775554)
    }

    #[test]
    fn test_sample_part2() {
        let res = part2(DATA);
        assert_eq!(res, 4174379265)
    }
}
