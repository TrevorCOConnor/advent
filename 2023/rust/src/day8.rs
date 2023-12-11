use num::integer::lcm;
use std::collections::HashMap;

static FILE_PATH: &'static str = "../data/day8.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 8");
    part1(&contents);
    part2(&contents);
    println!("Done");
}

enum Direction {
    LEFT,
    RIGHT 
}

fn parse_input(text: &str)
    -> (Vec<Direction>, HashMap<String, (String, String)>)
{
    let mut lines = text
        .lines()
        .filter(|l| !l.is_empty());

    let directions = lines
        .next().unwrap()
        .chars()
        .map(|c| match c {
           'R' => Direction::RIGHT,
           'L' => Direction::LEFT,
           _ => panic!("unknown direction {}", c)
        })
        .collect();

    let mut node_map: HashMap<String, (String, String)> = HashMap::new();
    for node_line in lines {
        let (key, nodes) = node_line.split_once('=')
            .expect("Node line does not meet expected format");

        let key = key.trim();
        let value = nodes
            .trim()
            .replace("(", "")
            .replace(")", "");

        let (left, right) = value.split_once(',')
            .expect("node format does not meet expectations");

        let node = (
            left.to_string(),
            right.trim().to_string()
        );

        node_map.insert(key.to_owned(), node);
    }

    (directions, node_map)
}


fn count_steps(
    directions: &[Direction],
    node_map: &HashMap<String, (String, String)>,
    start_position: &str,
    end_suffix: &str
) -> u32 {
    let mut current_position = start_position;
    let mut step_count = 0;
    for dir in directions
        .iter()
        .cycle()
    {
        step_count += 1;

        let new_node = node_map
            .get(current_position)
            .expect(
                &format!(
                    "Node not found in hashmap -{}-",
                    current_position
                )
            );

        current_position = match dir {
            Direction::LEFT => &new_node.0,
            Direction::RIGHT => &new_node.1
        };

        if current_position.ends_with(end_suffix) {
            break;
        }
    }

    step_count
}

fn ghost_steps(
    directions: &[Direction],
    node_map: &HashMap<String, (String, String)>,
    start_suffix: &str,
    end_suffix: &str
) -> u64 {
    let current_positions: Vec<String> = node_map.keys()
        .filter(|k| k.ends_with(start_suffix))
        .map(|k| k.to_owned())
        .collect();

    current_positions
        .into_iter()
        .map(|pos| count_steps(directions, node_map, &pos, end_suffix) as u64)
        .reduce(|x, y| lcm(x, y))
        .unwrap()

    
}

fn part1(contents: &str) -> u32 {
    let (directions, node_map) = parse_input(contents);
    let answer = count_steps(
        &directions,
        &node_map,
        "AAA",
        "ZZZ"
    );
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u64 {
    let (directions, node_map) = parse_input(contents);
    let answer = ghost_steps(
        &directions,
        &node_map,
        "A",
        "Z"
    );
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use crate::day8::part2;

    use super::part1;

    static FIRST_CASE: &'static str = "
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";
    static SECOND_CASE: &'static str = "
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
";
    static THIRD_CASE: &'static str = "
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
";

    #[test]
    fn test_part1() {
        let first_answer = part1(FIRST_CASE);
        assert_eq!(first_answer, 2);

        let second_answer = part1(SECOND_CASE);
        assert_eq!(second_answer, 6);
    }

    #[test]
    fn test_part2() {
        let answer = part2(THIRD_CASE);
        assert_eq!(answer, 6);
    }
}
