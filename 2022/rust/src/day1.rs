static FILE_PATH: &'static str = "../data/Day1.txt";

pub fn day1() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    let mut summations: Vec<i32> = contents
        .split("\n\n")
        .map(|x| {
            x.split("\n")
                .map(|x| x.parse::<i32>().unwrap_or(0)).sum()
        }).collect();
    summations.sort_by_key(|x| -x);
    println!("Day 1:");
    println!("Part 1: {:?}", summations[0]);
    println!("Part 2: {:?}", summations[..2].iter().sum::<i32>());
}
