import os

start = 1

def files_setup():
    for i in range(start, 26):
        file_contents = f"""static FILE_PATH: &'static str = "../data/day{i}.txt";

pub fn solutions() {{
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day {i}");
    part1(&contents);
    part2(&contents);
}}

fn part1(contents: &str) -> usize {{
    let answer = 0;
    // println!("Part 1: {{}}", answer);
    println!("Part 1: TODO");
    answer
}}

fn part2(contents: &str) -> usize {{
    let answer = 0;
    // println!("Part 2: {{}}", answer);
    println!("Part 2: TODO");
    answer
}}

#[cfg(test)]
mod test {{
    use super::{{part1, part2}};

    static DATA: &'static str = "
";

// #[test]
fn test_part1() {{
    let answer = part1(DATA);
    assert_eq!(answer, 0);
}}

// #[test]
fn test_part2() {{
    let answer = part2(DATA);
    assert_eq!(answer, 0);
}}

}}
    """
        with open(f"src/day{i}.rs", "w") as f:
            f.write(file_contents)

        with open(f"../data/day{i}.txt", "w") as f:
            f.write("")

def main_setup():
    contents = ""
    for i in range(start, 26):
        contents += f"mod day{i};\n"

    contents += """

use std::env;

fn main() {
    let args: Vec<String> = env::args().map(|x| x.to_lowercase()).collect();
    // Ignore target argument
    let args = &args[1..];

"""

    for i in range(1, 26):
        contents += f"""
    if args.contains(&String::from("day{i}")) || args.is_empty() {{
        day{i}::solutions();
    }}
"""

    contents += "}"

    with open("src/main.rs", "w") as f:
        f.write(contents)

files_setup()
main_setup()
