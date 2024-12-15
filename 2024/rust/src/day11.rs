use std::collections::HashMap;

static FILE_PATH: &'static str = "../data/day11.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 11");
    part1(&contents);
    part2(&contents);
}

fn read_data(contents: &str) -> Vec<u64> {
    contents
        .split(" ")
        .filter_map(|v| v.trim().parse::<u64>().ok())
        .collect()
}

fn apply_rock_rules(rock: &u64) -> Vec<u64> {
    if *rock == 0 {
        vec![1]
    } else {
        let rock_str = rock.to_string();
        if rock_str.len() % 2 == 0 {
            let (left, right) = rock_str.split_at(rock_str.len() / 2);
            vec![left.parse::<u64>().unwrap(), right.parse::<u64>().unwrap()]
        } else {
            vec![rock * 2024]
        }
    }
}

#[derive(Clone, Debug)]
enum Rock {
    Generation { parent: u64, rocks: Vec<Rock> },
    Reference { key: u64, index: usize },
    Base(u64),
}

impl Rock {
    fn new_base(value: u64) -> Self {
        Rock::Base(value)
    }

    fn new_ref(key: u64) -> Self {
        Rock::Reference { key, index: 0 }
    }

    // temp
    fn build_ref(key: u64, index: usize) -> Self {
        Rock::Reference { key, index }
    }

    fn new_generation(parent: u64, rocks: Vec<Rock>) -> Rock {
        Self::Generation { parent, rocks }
    }
}

struct Eyes {
    cache: HashMap<u64, Vec<usize>>,
    array: Vec<Rock>,
}

impl Eyes {
    fn build(array: Vec<u64>) -> Self {
        Eyes {
            cache: HashMap::new(),
            array: array.iter().map(|v| Rock::new_base(*v)).collect(),
        }
    }

    fn blink(&mut self) {
        let mut new_rocks = Vec::new();
        let array: Vec<Rock> = self.array.drain(..).collect();
        for rock in array {
            let new_rock = self.blink_rock(&rock);
            new_rocks.push(new_rock);
        }
        self.array = new_rocks;
    }

    fn calculate_cardinality(&self, rocks: &[Rock]) -> usize {
        let mut cardinality = 0;
        for rock in rocks {
            match rock {
                Rock::Generation {
                    parent: _,
                    rocks: gen_rocks,
                } => cardinality += self.calculate_cardinality(&gen_rocks),
                Rock::Base(_) => cardinality += 1,
                Rock::Reference { key, index } => {
                    cardinality += self.cache[key][*index];
                }
            }
        }
        cardinality
    }

    fn blink_rock(&mut self, rock: &Rock) -> Rock {
        match rock {
            // Handle generation of rocks recursively
            Rock::Generation { parent, rocks } => {
                let next_gen: Vec<Rock> = rocks.into_iter().map(|r| self.blink_rock(r)).collect();
                let cardinality = self.calculate_cardinality(&next_gen);
                self.cache.get_mut(parent).unwrap().push(cardinality);
                Rock::new_generation(*parent, next_gen)
            }

            // Handle single rock potentially breaking into a generation
            Rock::Base(value) => {
                if self.cache.contains_key(value) {
                    return Rock::build_ref(*value, 1);
                }
                let new_vals = apply_rock_rules(&value);
                if new_vals.len() == 1 {
                    let new_value = new_vals[0];
                    if self.cache.contains_key(&new_value) {
                        return Rock::new_ref(new_value);
                    } else {
                        return Rock::new_base(new_vals[0]);
                    }
                } else {
                    let mut new_rocks = Vec::new();
                    self.cache.insert(*value, vec![1, new_vals.len()]);
                    for val in new_vals {
                        if self.cache.contains_key(&val) {
                            new_rocks.push(Rock::new_ref(val));
                        } else {
                            new_rocks.push(Rock::new_base(val));
                        }
                    }
                    Rock::new_generation(*value, new_rocks)
                }
            }

            // Increment rock reference
            Rock::Reference { key, index } => Rock::Reference {
                key: *key,
                index: index + 1,
            },
        }
    }
}

fn part1(contents: &str) -> usize {
    let array = read_data(contents);
    let mut eyes = Eyes::build(array);
    for _ in 0..25 {
        eyes.blink();
    }
    let answer = eyes.calculate_cardinality(&eyes.array);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let array = read_data(contents);
    let mut eyes = Eyes::build(array);
    for _ in 0..75 {
        eyes.blink();
    }
    let answer = eyes.calculate_cardinality(&eyes.array);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::part1;

    static DATA: &'static str = "125 17";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 55312);
    }
}

