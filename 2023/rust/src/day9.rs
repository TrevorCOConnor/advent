static FILE_PATH: &'static str = "../data/day9.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 9");
    part1(&contents);
    part2(&contents);
}

fn parse_input(text: &str) -> Vec<Vec<i32>> {
    text
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l|
            l
            .split(' ')
            .map(|block| block
                .parse::<i32>()
                .expect(&format!("All values should be ints {}", block))
            )
            .collect()
        )
        .collect()
}

fn find_next_value(array: &[i32]) -> i32 {
    let difference_array: Vec<i32> = array
        .iter()
        .zip(array[1..].iter())
        .map(|(a, b)| b - a)
        .collect();

    let last_element = array
        .last()
        .expect("No array should be empty");

    if difference_array
        .iter()
        .all(|&v| v == 0)
    {
        *last_element
    } else {
        *last_element + find_next_value(&difference_array)    
    }
}

fn find_prev_value(array: &[i32]) -> i32 {
    let difference_array: Vec<i32> = array
        .iter()
        .zip(array[1..].iter())
        .map(|(a, b)| b - a)
        .collect();

    let first_element = array
        .first()
        .expect("No array should be empty");

    if difference_array
        .iter()
        .all(|&v| v == 0)
    {
        *first_element
    } else {
        *first_element - find_prev_value(&difference_array)    
    }
}

fn part1(contents: &str) -> i32 {
    let answer = parse_input(contents)
        .iter()
        .map(|l| find_next_value(l))
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> i32 {
    let answer = parse_input(contents)
        .iter()
        .map(|l| find_prev_value(l))
        .sum();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 114);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 2);
    }
}
