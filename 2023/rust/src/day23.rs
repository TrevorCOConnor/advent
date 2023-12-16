static FILE_PATH: &'static str = "../data/day23.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 23");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> usize {
    let answer = 0;
    // println!("Part 1: {}", answer);
    println!("Part 1: TODO");
    answer
}

fn part2(contents: &str) -> usize {
    let answer = 0;
    // println!("Part 2: {}", answer);
    println!("Part 2: TODO");
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
";

// #[test]
fn test_part1() {
    let answer = part1(DATA);
    assert_eq!(answer, 0);
}

// #[test]
fn test_part2() {
    let answer = part2(DATA);
    assert_eq!(answer, 0);
}

}
    