// This day is soup. I will definitely be cleaning it up

use core::fmt;
use std::collections::HashMap;

static FILE_PATH: &'static str = "../data/day12.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken file.");

    println!("Day 12");
    part1(&contents);
    part2(&contents, 5);
}

fn read_spring_map(map_str: &str) -> Vec<Option<bool>> {
    map_str
        .chars()
        .map( |c| match c {
            '?' => None,
            '.' => Some(false),
            '#' => Some(true),
              _ => panic!("Unknown character {c}"),
        })
        .collect()
}

fn read_line(line: &str) -> (Vec<Option<bool>>, Vec<usize>) {
    let (spring_map_str, counts_str) = line.split_once(' ').unwrap();
    (
        read_spring_map(spring_map_str.trim()),
        counts_str.trim().split(',')
            .map(|v| v.parse::<usize>().unwrap())
            .collect()
    )
}

fn read_text(text: &str) -> Vec<(Vec<Option<bool>>, Vec<usize>)> {
    text.lines()
        .filter(|l| !l.is_empty())
        .map(|l| read_line(l))
        .collect()
}

struct SpringCache(HashMap<Vec<Option<bool>>, HashMap<Vec<usize>, usize>>);

impl SpringCache {
    fn new() -> SpringCache {
        SpringCache(HashMap::new())
    }

    fn find(
        &self,
        spring_line: &[Option<bool>],
        counts: &[usize]
    ) -> Option<usize> {
        if self.0.contains_key(spring_line) {
            let inner = &self.0[spring_line];
            if inner.contains_key(counts) {
                return Some(inner[counts])
            }
        }
        None
    }

    fn add(&mut self, spring_line: &[Option<bool>], counts: &[usize], result: usize) {
        if let None = self.find(spring_line, counts) {
            if let Some(inner_cache) = self.0.get_mut(spring_line) {
                inner_cache.insert(counts.to_owned(), result);
            } else {
                let inner_cache = HashMap::from(
                    [(counts.to_owned(), result)]
                );
                self.0.insert(spring_line.to_owned(), inner_cache);
            }
        } 
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
struct SpringLine{
    line: Vec<Option<bool>>,
    counts: Vec<usize>
}

impl fmt::Display for SpringLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line: String = self.line.iter()
            .map(|v| match v {
                None => '?',
                Some(true) => '#',
                Some(false) => '.'
            })
            .collect();
        write!(f, "{} // {:?}", line, self.counts)
    }
}

impl SpringLine {
    fn satisfies_counts(&self) -> bool {
        if self.line.contains(&None) {
            panic!("line cannot be evaluated with unknown values")
        }

        let found_counts: Vec<usize> = self.line
            .split(|v| v.is_some_and(|inner| !inner))
            .filter_map(|v| if v.len() > 0 {Some(v.len())} else {None})
            .collect();

        found_counts == self.counts
    }

    fn set_next(&self, value: bool) -> SpringLine {
        let next_index = self.line
            .iter()
            .enumerate()
            .find_map(|(e, v)| if v.is_none() {Some(e)} else {None})
            .expect("Cannot find next unknown");

        let new_line = self.line
            .iter()
            .enumerate()
            .map(|(e, v)| if e == next_index {Some(value.clone())} else {v.clone()})
            .collect();

        SpringLine { line: new_line, counts: self.counts.clone() }
    }

    fn remove_contiguous_dots(&mut self) {
        self.line = self.line.iter().zip(
            self.line[1..].iter().chain([None].iter())
        )
            .filter_map(|(current, next)| {
                if let Some(false) = current {
                    if let Some(false) = next {
                        None
                    } else {
                        Some(*current)
                    }
                } else {
                    Some(*current)
                }
            })
            .collect()
    }

    fn reduce(&mut self) -> bool {
        self.remove_contiguous_dots();

        let mut current_group: usize = 0;
        let mut counts_iter = self.counts.drain(0..);
        let mut drop_idx: usize = 0;

        let mut next_count = counts_iter.next();

        // This is messy
        // We are collecting groups of `#`s and comparing them against the counts
        // If they match, we drop them and the corresponding count.
        // We stop when we hit an unknown value.
        for (idx, value) in self.line.iter().enumerate() {
            match value {
                Some(true) => {
                    current_group += 1;
                    if next_count.map(|v| v < current_group)
                        .unwrap_or(true)
                    {
                            return false
                    }
                },
                Some(false) => {
                    if current_group > 0 {
                        if let None = next_count {
                            return false
                        }
                        let group_size = next_count.unwrap();
                        if current_group.ne(&group_size) {
                            return false
                        }
                        current_group = 0;
                        next_count = counts_iter.next();
                        drop_idx = idx;
                    }
                },
                None => {
                    break;
                }
            }
        }

        self.line = self.line[drop_idx..].to_owned();
        self.counts = [next_count]
            .into_iter()
            .filter_map(|v| v)
            .chain(counts_iter).collect();
        true
    }

    fn calculate_spring_map_possibilities(
        &self,
        cache: &mut SpringCache,
    ) -> usize {
        // Return cached result if possible
        if let Some(result) = cache.find(&self.line, &self.counts) {
            return result
        }

        // If there are no more decisions to be made
        // return whether or not its successful
        if !self.line.contains(&None) {
            if self.satisfies_counts() {
                return 1
            } else {
                return 0
            }
        }

        // Check with next unknown as `true`
        let mut true_line = self.set_next(true);
        let reduced = true_line.reduce();
        let true_scenario = {
            if reduced {
                let scenario = true_line.calculate_spring_map_possibilities(cache);
                cache.add(&true_line.line, &true_line.counts, scenario);
                scenario
            } else {
                0
            }
        };

        // Check with next unknown as `false`
        let mut false_line = self.set_next(false);
        let reduced = false_line.reduce();
        let false_scenario = {
            if reduced {
                let scenario = false_line.calculate_spring_map_possibilities(cache);
                cache.add(&false_line.line, &false_line.counts, scenario);
                scenario
            } else {
                0
            }
        };

        false_scenario + true_scenario
    }

    fn unfold(&self, fold_count: usize) -> SpringLine {
        let new_counts = (0..fold_count).into_iter()
            .fold(Vec::new(), |mut acc, _| {
                acc.extend_from_slice(&self.counts);
                acc
            });

        let new_line = (0..fold_count).into_iter()
            .fold(Vec::new(), |mut acc, _| {
                acc.push(None);
                acc.extend_from_slice(&self.line);
                acc
            });

        let new_line = new_line[1..].to_owned();

        SpringLine { line: new_line, counts: new_counts }
    }
}

fn part1(contents: &str) -> usize {
    let mut cache = SpringCache::new();
    let answer = read_text(contents)
        .into_iter()
        .map(|(spring_line, counts)|
            SpringLine { line: spring_line, counts}.calculate_spring_map_possibilities(&mut cache)
        )
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str, fold_count: usize) -> usize {
    let mut cache = SpringCache::new();
    let answer = read_text(contents)
        .into_iter()
        .map(|(spring_line, counts)| {
            let counts = 
            SpringLine { line: spring_line, counts}
            .unfold(fold_count)
            .calculate_spring_map_possibilities(&mut cache);
            println!("{counts}");
            counts
        })
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 21)
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA, 5);
        assert_eq!(answer, 525152)
    }

}
