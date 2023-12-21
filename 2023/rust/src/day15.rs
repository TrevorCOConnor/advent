use std::collections::hash_map;

static FILE_PATH: &'static str = "../data/day15.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 15");
    part1(&contents);
    part2(&contents);
}

fn read_input(text: &str) -> Vec<String> {
    text.split(',')
        .map(|chunk| chunk
            .chars()
            .filter(|c| *c != '\n')
            .collect()
        )
        .collect()
}

fn read_input_to_instructions(text: &str) -> Vec<(String, Option<usize>)> {
    text.split(',')
        .map(|chunk| chunk
            .chars()
            .filter(|c| *c != '\n')
            .collect::<String>()
        )
        .map(|chunk|
            if chunk.contains("-") {
                (chunk.strip_suffix("-").unwrap().to_owned(), None)
            } else {
                let (left, right) = chunk.split_once("=").unwrap();
                (left.to_owned(), Some(right.to_owned().parse::<usize>().unwrap()))
            }
        )
        .collect()
}

fn ascii_hash(text: &str) -> usize {
    text
        .chars()
        .fold(0, |acc, new| {
            ((acc + (new as usize)) * 17).rem_euclid(256)
        })
}

// Intentionally not using a real hashmap
// that's obviously cheating
#[derive(Debug)]
struct HashMap(Vec<Vec<(String, usize)>>);

impl HashMap {
    fn new() -> HashMap {
        HashMap(
            vec![Vec::new(); 2usize.pow(8)]
        )
    }

    fn add(&mut self, label: &str, value: &usize) {
        let hash = ascii_hash(label);
        let values = &self.0[hash];

        // little clunky
        if let Some(existing_idx) = values
            .iter()
            .enumerate()
            .find(|(_, (lab, _))| lab == label)
            .map(|(e, _)| e)
        {
            self.0[hash][existing_idx] = (label.to_owned(), value.to_owned());
        } else {
            self.0[hash].push((label.to_string(), value.to_owned()));
        }
    }

    fn delete(&mut self, label: &str) {
        let hash = ascii_hash(label);
        self.0[hash] = self.0[hash]
            .drain(..)
            .filter(|(lab, _)| lab != label)
            .collect();
    }

    fn focusing_power(&self) -> usize {
        self.0
            .iter()
            .enumerate()
            .map(|(box_number, vec)| {
                vec
                    .iter()
                    .enumerate()
                    .map(|(slot_number, (_, focal_length))| {
                        (box_number + 1) * (slot_number + 1) * focal_length
                    })
                    .sum::<usize>()
            })
            .sum()
    }
}

fn part1(contents: &str) -> usize {
    let answer = read_input(contents)
        .into_iter()
        .map(|chunk| ascii_hash(&chunk))
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let instructions = read_input_to_instructions(contents);
    let mut hash_map = HashMap::new();
    for (label, maybe_value) in instructions {
        if let Some(value) = maybe_value {
            hash_map.add(&label, &value)
        } else {
            hash_map.delete(&label)
        }
    }

    let answer = hash_map.focusing_power();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use crate::day15::ascii_hash;

    use super::{part1, part2};

    static DATA: &'static str = "
rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 1320);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 145);
    }

}
    
