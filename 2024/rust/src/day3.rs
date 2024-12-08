use regex::Regex;

static FILE_PATH: &'static str = "../data/day3.txt";


pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 3");
    part1(&contents);
    part2(&contents);
}

fn parse_instructions(contents: &str) -> u32 {
    let re = Regex::new(r"mul\((?<fst>\d{1,3}),(?<snd>\d{1,3})\)").unwrap();

    re.captures_iter(contents).map(|c| 
        &c["fst"].parse::<u32>().unwrap()
            * &c["snd"].parse::<u32>().unwrap()
    ).sum()
}

fn parse_instructions_with_conditionals(contents: &str) -> u32 {
    let re = Regex::new(r"(mul\((?<fst>\d{1,3}),(?<snd>\d{1,3})\))|(?<dont>don\'t)|(?<do>do)").unwrap();

    let mut enabled = true;
    let mut sum = 0;

    for cap in re.captures_iter(contents) {
        if cap.name("dont").is_some() {
            enabled = false;
        } else if cap.name("do").is_some() {
            enabled = true;
        } else if enabled {
            sum += &cap["fst"].parse::<u32>().unwrap()
                * &cap["snd"].parse::<u32>().unwrap();
        }
    }
    sum
}

fn part1(contents: &str) -> u32 {
    let answer = parse_instructions(contents);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let answer = parse_instructions_with_conditionals(contents);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    static DATA2: &'static str = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5)";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 161);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA2);
        assert_eq!(answer, 48);
    }
}
