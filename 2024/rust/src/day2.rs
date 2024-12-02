static FILE_PATH: &'static str = "../data/day2.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 2");
    part1(&contents);
    part2(&contents);
}

fn parse_data(data: &str) -> Vec<Vec<i32>> {
    data.lines()
        .filter(|l| !l.is_empty())
        .map(
            |l| l.split(" ").map(|num| num.parse::<i32>().expect("Not a number!")).collect()
        ).collect()
}

fn validate_report(report: &[i32]) -> bool {
    if report.len() < 2 {
        return true
    }
    let sign = (report[0] - report[1]).signum();
    report.iter()
        .zip(report[1..].iter())
        .all(|(fst, snd)| {
            let diff = fst - snd;
            // Diff is not greater than 3
            // Sign (asc/ desc) remains the same
            diff.abs() <= 3 && diff.signum() * sign == 1
        })
}

fn validate_report_with_dampener(report: &[i32]) -> bool {
    if validate_report(report) {
        return true;
    }
    let analyzed_report = report.iter()
        .zip(report[1..].iter())
        .map(|(fst, snd)| {
            let diff = snd - fst;
            if diff.abs() > 3 {
                0
            } else {
                diff.signum()
            }
        });

    let mut neg = Vec::new();
    let mut pos = Vec::new();
    let mut zero = Vec::new();
    for (idx, diff) in analyzed_report.enumerate() {
        match diff {
            -1 => neg.push(idx),
            1 => pos.push(idx),
            _ => zero.push(idx)
        }
    }

    let mut discrepancies =  {
        if neg.len() >= pos.len() {
            pos
        } else {
            neg
        }
    };
    discrepancies.extend(zero);

    // If there is only one discrepancy or two neighboring discrepancies,
    // the report could potentially be fixed
    // We're not going to check if the two are neighboring though
    if discrepancies.len() > 2 {
        return false
    }

    for idx in discrepancies {
        let first = report[..idx].iter().cloned().chain(report[idx+1..].iter().cloned()).collect::<Vec<i32>>();
        if validate_report(&first) {
            return true;
        }
        let second = report[..idx+1].iter().cloned().chain(report[idx+2..].iter().cloned()).collect::<Vec<i32>>();
        if validate_report(&second) {
            return true;
        }
    }

    false

}

fn part1(contents: &str) -> usize {
    let reports = parse_data(contents);
    let answer = reports.iter().filter(|r| validate_report(r)).count();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let reports = parse_data(contents);
    let answer = reports.iter().filter(|r| validate_report_with_dampener(r)).count();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 2);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 4);
    }

}
    
