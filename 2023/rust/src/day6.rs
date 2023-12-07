static FILE_PATH: &'static str = "../data/day6.txt";

pub fn solutions() {
    println!("Day 6");
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    part1(&contents);
    part2(&contents);
}


fn evaluate_winning_scenarios(time: u64, distance: u64) -> u64 {
    let time = time as f64;
    let distance = distance as f64;

    // Quadratic formula, baby!
    let a = (time - f64::sqrt((time * time) - 4.0*distance)) / 2.0;
    let b = (time + f64::sqrt((time * time) - 4.0*distance)) / 2.0;

    // Smallest int above `a` (different than ceiling)
    let a = (a.max(1.0) + 1.0).floor() as u64;

    // Greatest int below `b` (different than floor)
    let b = (b - 1.0).ceil() as u64;

    // range of numbers between `a` and `b`
    1 + (b - a)
}

fn read_data_part1(text: &str) -> Vec<(u64, u64)> {
    let mut lines = text
        .lines()
        .filter(|l| !l.is_empty());

    let times = lines
        .next()
        .unwrap()
        .split(' ')
        .filter_map(|v| v.parse::<u64>().ok());

    let distances = lines
        .next()
        .unwrap()
        .split(' ')
        .filter_map(|v| v.parse::<u64>().ok());

    times.zip(distances).collect()
}

fn read_data_part2(text: &str) -> (u64, u64) {
    let mut lines = text
        .lines()
        .filter(|l| !l.is_empty());

    let time: u64 = lines
        .next().unwrap()
        .split(':')
        .last().unwrap()
        .replace(" ", "")
        .parse().unwrap();

    let distance: u64 = lines
        .next().unwrap()
        .split(':')
        .last().unwrap()
        .replace(" ", "")
        .parse().unwrap();

    (time, distance)
}


fn part1(text: &str) -> u64 {
    let answer = read_data_part1(text)
        .into_iter()
        .map(|(time, distance)| evaluate_winning_scenarios(time, distance))
        .product();
    println!("Part 1: {}", answer);
    answer
}

fn part2(text: &str) -> u64 {
    let (time, distance) = read_data_part2(text);
    let answer = evaluate_winning_scenarios(time, distance);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
Time:      7  15   30
Distance:  9  40  200
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 288)
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 71503)
    }
}
