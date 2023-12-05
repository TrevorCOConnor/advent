static FILE_PATH: &'static str = "../data/day4.txt";

use std::collections::VecDeque;

use regex::Regex;

pub fn solutions() {
    println!("Day 4");
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    part1(&contents);
    part2(&contents);
}

fn str_to_bitmap(line: &str) -> u128 {
    let re = Regex::new(r"\d+").unwrap();
    re.find_iter(line)
        .map(|m| m.as_str().parse::<u32>().unwrap())
        .fold(0, |acc, new| acc | 1 << new)
}

fn calculate_matches(line: &str) -> u32 {
    let (winning_str, present_str) = line
        .split_once(':')
        .unwrap()
        .1
        .split_once('|')
        .unwrap();
    let winning_numbers = str_to_bitmap(winning_str);
    let present_numbers = str_to_bitmap(present_str);
    (winning_numbers & present_numbers).count_ones()
}

fn calculate_copies(text: &str) -> u32 {
    let mut copies: Vec<u32> = Vec::new();
    let mut queue: VecDeque<u32> = VecDeque::new();

    let lines = text
        .lines()
        .filter(|l| !l.is_empty());

    for line in lines {
        let copy_number = queue.pop_front().unwrap_or(0) + 1;
        copies.push(copy_number);
        let matches = calculate_matches(line);
        for i in 0..matches {
            let index = i as usize;
            if queue.len() <= index {
                queue.push_back(copy_number);
            } else {
                queue[index] += copy_number;
            }
        }
    }

    copies.iter().sum()
}

fn part1(contents: &str) -> u32{
    let answer = contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            let matches = calculate_matches(l);
            if matches >= 1 { 2_u32.pow(matches - 1) } else { 0 }
        })
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let answer = calculate_copies(contents);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::*;

    static DATA: &'static str = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 13)
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 30)
    }
}
