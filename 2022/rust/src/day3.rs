use std::fmt::format;

struct Rucksack(Compartment, Compartment);

impl Rucksack {
    fn new(text: String) -> Rucksack {
        let compartment_length = text.len().checked_div(2).expect("Rucksack should have even length");
        Rucksack(
            Compartment::new(text[0..compartment_length].to_string()),
            Compartment::new(text[compartment_length..].to_string())
        )
    }

    fn shared_priority(&self) -> u32 {
        self.0.compare(&self.1)
    }
}

struct Compartment(Vec<Item>);

impl Compartment {
    fn new(items: String) -> Compartment {
        return Compartment(
            items.chars().map(|c| Item(c)).collect()
        )
    }

    fn unique_items(&self) -> u64 {
        // Creates a bit map of unique_items
        // using bit-wise or
        self.0.iter().fold(
            0u64,
            |acc, n| acc | (1 << n.priority())
        )
    }

    fn compare(&self, other: &Compartment) -> u32 {
        // Uses a bit-wise and to find shared items
        // and then counting the zeros
        let and = self.unique_items() & other.unique_items();
        println!("{:b}", and);
        u64::trailing_zeros(
            self.unique_items() & other.unique_items()
        )
    }
}

struct Item(char);

impl Item {
    fn priority(&self) -> u32 {
        if self.0.is_lowercase() {
            (self.0 as u32) - ('a' as u32) + 1
        } else {
            (self.0 as u32) - ('A' as u32) + 27
        }
    }
}

fn part1(text: String) -> u32 {
    text.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| Rucksack::new(l.trim().to_string()))
        .map(|r| r.shared_priority())
        .sum()
}

pub fn day3(text: String) {
    let part1: u32 = part1(text);
    println!("Day 3:");
    println!("Part 1: {:?}", part1);
    // println!("Part 2: {:?}", part2);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_priority() {
        let item = Item('a');
        println!("a: {}", item.priority());
        assert!(item.priority() == 1);

        let item = Item('A');
        println!("A: {}", item.priority());
        assert!(item.priority() == 28)
    }

    #[test]
    fn test_sample() {
        let example_data = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
        ".to_string();
        let prt1 = part1(example_data);
        println!("{}", prt1);
        assert_eq!(prt1, 157)
    }
}
