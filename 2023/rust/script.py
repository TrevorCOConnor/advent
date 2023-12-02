import os

def files_setup():
    for i in range(1, 26):
        file_contents = f"""static FILE_PATH: &'static str = "../data/Day{i}.txt";

    pub fn solutions() {{
        println!("Day {i}");
        part1();
        part2();
    }}

    fn part1() {{
        let answer = "TODO";
        println!("Part 1: {{}}", answer)
    }}

    fn part2() {{
        let answer = "TODO";
        println!("Part 2: {{}}", answer)
    }}
    """
        with open(f"src/day{i}.rs", "w") as f:
            f.write(file_contents)

        with open(f"../data/day{i}.txt", "w") as f:
            f.write("")

def main_setup():
    contents = ""
    for i in range(1, 26):
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
    print(contents)

    with open("src/main.rs", "w") as f:
        f.write(contents)

main_setup()
