static FILE_PATH: &'static str = "../data/day1.txt";

fn read_data(contents: &str) -> (Vec<u32>, Vec<u32>) {
    let lines = contents.lines()
        .filter(|l| !l.trim().is_empty());

    let mut left = Vec::new();
    let mut right = Vec::new();
    for line in lines {
        let (first, second) = line.trim()
            .split_once(" ")
            .expect("Line does not have two numbers");

        left.push(first.parse::<u32>().expect("Value on left is not a number"));
        right.push(second.trim().parse::<u32>().expect("Value on right is not a number"));
    }

    (left, right)
}

fn calculate_list_diff(left: &[u32], right: &[u32]) -> u32 {
    let mut left_sort = left.to_owned();
    left_sort.sort();

    let mut right_sort = right.to_owned();
    right_sort.sort();

    left_sort.iter().zip(right_sort.iter())
        .map(|(&l, &r)| l.abs_diff(r))
        .sum()
}

fn calculate_list_sim(left: &[u32], right: &[u32]) -> u32 {
    let mut left_sort = left.to_owned();
    left_sort.sort();

    let mut right_sort = right.to_owned();
    right_sort.sort();

    let mut sum = 0u32;
    // Track location in right list
    // This is only useful because the list is sorted
    let mut idx = 0usize;

    for item in left_sort {
        // Find index of matching item in right list
        let new_idx = right_sort[idx..]
            .iter()
            .position(|e| *e == item);

        // If matching item, count items
        if let Some(n_idx) = new_idx {
            idx = idx + n_idx;
            let count = right_sort[idx..]
                .iter()
                .take_while(|e| **e == item)
                .count();
            sum += (count as u32) * item;
        }

        // // Takes advantage of sorted lists
        // let count = right_sort.iter()
        //     // Skip lower numbers
        //     .skip_while(|&&r| item > r)
        //     // Count all equal numbers, and then stop
        //     .take_while(|&&r| item == r).count() as u32;
        // sum += count * item;
    }

    sum
}

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 1");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let (left, right) = read_data(contents);
    let answer = calculate_list_diff(&left, &right);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let (left, right) = read_data(contents);
    let answer = calculate_list_sim(&left, &right);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
        3   4
        4   3
        2   5
        1   3
        3   9
        3   3
    ";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 11);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 31);
    }
}
