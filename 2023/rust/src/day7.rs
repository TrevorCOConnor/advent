use std::collections::HashMap;

use itertools::Itertools;

static FILE_PATH: &'static str = "../data/day7.txt";

const JOKER: u32 = 11;

fn read_line(line: &str) -> (Vec<u32>, u32) {
    let (hand, bid) = line
        .split(' ')
        .collect_tuple::<(&str, &str)>()
        .unwrap();

    let translated_hand = hand.chars()
        .map(|c| {
            if let Some(number) = c.to_digit(10) {
                number
            } else {
                match c {
                    'T' => 10,
                    'J' => 11,
                    'Q' => 12,
                    'K' => 13,
                    'A' => 14,
                    _ => panic!("Invalid card found {}", c)
                }
            }
        })
        .collect();

    let translated_bid = bid.parse().unwrap();

    (translated_hand, translated_bid)
}

fn read_input(contents: &str) -> Vec<(Vec<u32>, u32)> {
    contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(
            |line| read_line(line) 
        )
        .collect()
}

fn hand_type_rank(hand: &[u32]) -> (u8, u8, u32, u32, u32, u32, u32) {
    let mut card_map: HashMap<u32, u8> = HashMap::with_capacity(5);
    for card in hand.iter() {
        *card_map.entry(*card).or_insert(0) += 1
    };
    let mut matches = card_map
        .values()
        .sorted()
        .rev();

    (
        *matches.next().unwrap(),
        *matches.next().unwrap_or(&0),
        hand[0],
        hand[1],
        hand[2],
        hand[3],
        hand[4]
    )
}

fn hand_type_rank_with_jokers(hand: &[u32]) -> (u8, u8, u32, u32, u32, u32, u32) {
    let mut card_map: HashMap<u32, u8> = HashMap::with_capacity(5);
    for card in hand.iter() {
        *card_map.entry(*card).or_insert(0) += 1
    };
    let number_of_jokers = card_map.remove(&JOKER).unwrap_or(0);
    let mut matches = card_map
        .values()
        .sorted()
        .rev();

    let adjusted_hand: Vec<u32> = hand
        .iter()
        .map(|v| if v == &JOKER { 0 } else { *v })
        .collect();

    // I am making the assumption that you should just always make the jokers
    // the same as the card with the highest number already
    (
        *matches.next().unwrap_or(&0) + number_of_jokers,
        *matches.next().unwrap_or(&0),
        adjusted_hand[0],
        adjusted_hand[1],
        adjusted_hand[2],
        adjusted_hand[3],
        adjusted_hand[4]
    )
}

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    println!("Day 7");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let input = read_input(contents);

    let answer = input
        .iter()
        .sorted_by_key(|hand| hand_type_rank(&hand.0))
        .enumerate()
        .map(|(rank, (_, bid))| (rank as u32 + 1) * bid)
        .sum();

    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let input = read_input(contents);

    let answer = input
        .iter()
        .map(|(hand, bid)| (hand_type_rank_with_jokers(hand), bid))
        .sorted_by_key(|hand| hand.0)
        .enumerate()
        .map(|(rank, (_, bid))| (rank as u32 + 1) * bid)
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use crate::day7::part2;

    use super::part1;

    static DATA: &'static str = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
";
    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 6440)
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 5905)
    }
}
