use std::collections::{HashMap, HashSet};

static FILE_PATH: &'static str = "../data/day5.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 3");
    part1(&contents);
    part2(&contents);
}

struct RuleMap {
    before: HashSet<u32>,
    after: HashSet<u32>
}

impl RuleMap {
    fn new() -> RuleMap {
        RuleMap { before: HashSet::new(), after: HashSet::new() }
    }
}

fn read_data(contents: &str) -> (HashMap<u32, RuleMap>, Vec<Vec<u32>>) {
    let mut hashmap = HashMap::new();
    let mut updates = Vec::new();


    let rules = contents.lines().take_while(|l| !l.is_empty());
    for rule in rules {
        let (before, after) = rule.trim().split_once("|").expect("Rule improperly formatted");
        let before = before.parse::<u32>().expect("Non number rule");
        let after = after.parse::<u32>().expect("Non number rule");
        
        if !hashmap.contains_key(&before) {
            hashmap.insert(before, RuleMap::new());
        }
        hashmap.get_mut(&before).unwrap().after.insert(after);

        if !hashmap.contains_key(&after) {
            hashmap.insert(after, RuleMap::new());
        }
        hashmap.get_mut(&after).unwrap().before.insert(before);
    }

    let mut update_commands = contents.lines().skip_while(|l| !l.is_empty());
    update_commands.next();
    for update in update_commands {
        let row: Vec<u32> = update.trim().split(",").map(|v| v.parse::<u32>().unwrap()).collect();
        updates.push(row);
    }

    (hashmap, updates)
}

fn verify_update(update: &[u32], rule_map: &HashMap<u32, RuleMap>) -> bool {
    for idx in 0..update.len() {
        let val = update[idx];
        let before = HashSet::from_iter(update[..idx].iter().cloned());
        let after = HashSet::from_iter(update[idx+1..].iter().cloned());

        let before_rules = &rule_map.get(&val).unwrap().before;
        let after_rules = &rule_map.get(&val).unwrap().after;

        if before.intersection(after_rules).count() != 0 {
            return false
        }
        
        if after.intersection(before_rules).count() != 0 {
            return false
        }
    }
    true
}

fn reorder_update(update: Vec<u32>, rule_map: &HashMap<u32, RuleMap>) -> Vec<u32> {
    for idx in 0..update.len() {
        let val = update[idx];
        let after = HashSet::from_iter(update[idx+1..].iter().cloned());

        let before_rules = &rule_map.get(&val).unwrap().before;

        let incorrect: Vec<u32> = after.intersection(before_rules).cloned().collect();
        if incorrect.len() > 0 {
            let correct = after.difference(before_rules).cloned().collect::<Vec<u32>>();
            let adjusted = [&update[..idx], &incorrect, &[val] , &correct].concat();
            return reorder_update(adjusted, rule_map);
        }
    }
    update
}

fn get_middle(update: &[u32]) -> u32 {
    let mid = update.len().div_euclid(2);
    update[mid]
}

fn part1(contents: &str) -> u32 {
    let (rule_map, updates) = read_data(contents);
    let answer = updates.iter()
        .filter(|u| verify_update(u, &rule_map))
        .map(|u| get_middle(u))
        .sum::<u32>();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let (rule_map, updates) = read_data(contents);
    let answer = updates.iter()
        .filter(|u| !verify_update(u, &rule_map))
        .map(|u| reorder_update(u.clone(), &rule_map))
        .map(|u| get_middle(&u))
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 143);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 123);
    }
}

