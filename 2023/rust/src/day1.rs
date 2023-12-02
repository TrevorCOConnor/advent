static FILE_PATH: &'static str = "../data/day1.txt";

pub fn solutions() {
    println!("Day 1");
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    part1(&contents);
    part2(&contents);
}


fn convert_text_to_digits(line: &str) -> String {
    line.replace("one", "o1e")
        .replace("two", "t2o")
        .replace("three", "t3e")
        .replace("four", "f4r")
        .replace("five", "f5e")
        .replace("six", "s6x")
        .replace("seven", "s7n")
        .replace("eight", "e8t")
        .replace("nine", "n9e")
}

enum Part {
    One,
    Two
}

fn evaluate(contents: &str, part: Part) -> u32 {
    let answer = contents
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            let l = match part {
                Part::One => l.to_owned(),
                Part::Two => convert_text_to_digits(l)
            };
            let digits = l.chars().filter_map(
                |c| c.to_digit(10)
            ).collect::<Vec<u32>>();
            let first = digits
                .iter()
                .next()
                .expect("A digit should exist in the line");
            let second = digits.iter().last().unwrap();
            first * 10 + second
        }).sum::<u32>();
    answer
}

fn part1(contents: &str) -> u32 {
    let answer = evaluate(contents, Part::One);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let answer = evaluate(contents, Part::Two);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_part1() {
        let data = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";
        let answer = part1(data);
        assert_eq!(answer, 142);
    }

    #[test]
    fn test_part2() {
        let data = "
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";
        let answer = part2(data);
        assert_eq!(
            answer, 281
        )
    }
}
