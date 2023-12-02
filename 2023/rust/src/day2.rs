static FILE_PATH: &'static str = "../data/day2.txt";

pub fn solutions() {
    println!("Day 2");
    let contents = std::fs::read_to_string(FILE_PATH)
        .expect("Broken file");
    part1(&contents);
    part2(&contents);
}

enum Color {
    Red,
    Blue,
    Green
}

#[derive(Debug)]
struct Round {
    red: u32,
    blue: u32,
    green: u32
}

impl Round {
    fn new() -> Round {
        Round { red: 0, blue: 0, green: 0 }
    }

    fn find_color(color: &str, results: &[(u32, &str)]) -> u32 {
        results
            .iter()
            .find(|(_, c)| c == &color)
            .map(|t| t.0)
            .unwrap_or(0)
    }

    fn parse_from_line(line: &str) -> Round {
        let tuples = line.split(',')
            .map(
                |chunk| {
                    let tup = chunk
                        .trim()
                        .split_once(' ')
                        .expect("Should be '# color'");
                    (
                        tup.0.trim().parse::<u32>().unwrap(),
                        tup.1.trim()
                    )
                }
            )
            .collect::<Vec<(u32, &str)>>();
        
        let r = Round {
            red: Round::find_color("red", &tuples),
            blue: Round::find_color("blue", &tuples),
            green: Round::find_color("green", &tuples)
        };
        r
    }

    fn max(&self, other: &Round) -> Round {
        Round {
            red: self.red.max(other.red),
            blue: self.blue.max(other.blue),
            green: self.green.max(other.green)
        }
    }

    fn any_gt(&self, other: &Round) -> bool {
        self.red.gt(&other.red)
            | self.blue.gt(&other.blue)
            | self.green.gt(&other.green)
    }

    fn power(&self) -> u32 {
        self.red * self.blue * self.green
    }
}

struct Game(Vec<Round>);

impl Game {
    fn parse_from_line(line: &str) -> Game {
        Game(
            line.split_once(':') // Drop Game prefix
                .expect("Line should start with Game prefix")
                .1 // 
                .split(';')
                .map(
                    |round| Round::parse_from_line(round)
                )
                .collect()
        )
    }

    fn fold(&self) -> Round {
        self.0.iter()
            .fold(
                Round::new(),
                |acc, e| acc.max(e)
            )
    }
}


fn part1(contents: &str) -> u32 {
    let test = Round { red: 12, green: 13, blue: 14 };
    let answer = contents.lines()
        .filter(|l| !l.is_empty())
        .map(|l| Game::parse_from_line(l).fold())
        .enumerate()
        .filter_map(|(e, round)| 
            if !round.any_gt(&test) { Some(e+1) } else {None}
        )
        .sum::<usize>();
    println!("Part 1: {}", answer);
    answer as u32
}

fn part2(contents: &str) -> u32 {
    let answer = contents.lines()
        .filter(|l| !l.is_empty())
        .map(|l| Game::parse_from_line(l).fold().power())
        .sum::<u32>();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        let data = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";
    let answer = part1(data);
    assert_eq!(answer, 8)
    }

    #[test]
    fn test_part2() {
        let data = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";
    let answer = part2(data);
    assert_eq!(answer, 2286)
    }
}
