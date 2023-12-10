static FILE_PATH: &'static str = "../data/day9.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 9");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let answer = "TODO";
    println!("Part 1: {}", answer)
}

fn part2(contents: &str) -> u32 {
    let answer = "TODO";
    println!("Part 2: {}", answer)
}

#[cfg(test)]
mod test {
    use crate::day9::part1;

    static DATA: &'static str = "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, )
    }

    #[test]
    fn test_part2() {
    }
}
