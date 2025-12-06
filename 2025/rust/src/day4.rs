use rust::{Matrix, Position};

const FILE_PATH: &'static str = "../data/day4.txt";

fn parse_input(inp: &str) -> Matrix<bool> {
    let rows = inp
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            l.chars()
                .map(|c| if c == '@' { true } else { false })
                .collect()
        })
        .collect();

    Matrix::new(rows)
}

fn find_candidates(map: &Matrix<bool>) -> Vec<Position> {
    map.values_iter()
        .filter(|v| {
            *v.value
                && map
                    .value_neighbors_iter(&v.pos)
                    .filter(|n| *n.value)
                    .take(4)
                    .count()
                    < 4
        })
        .map(|v| v.pos)
        .collect()
}

fn remove_candidates(map: &mut Matrix<bool>, pos: &[Position]) {
    for p in pos {
        map.set(p, false);
    }
}

fn part1(file_contents: &str) -> usize {
    let mtrx = parse_input(file_contents);
    find_candidates(&mtrx).iter().count()
}

fn part2(file_contents: &str) -> usize {
    let mut mtrx = parse_input(file_contents);
    let mut total = 0;
    loop {
        let positions: Vec<Position> = find_candidates(&mtrx).into_iter().collect();
        let count = positions.len();

        total += count;
        remove_candidates(&mut mtrx, &positions);
        if count == 0 {
            break;
        }
    }
    total
}

pub fn solution() -> Result<(), Box<dyn std::error::Error>> {
    let contents = std::fs::read_to_string(FILE_PATH)?;
    println!("Day 3");
    let p1 = part1(&contents);
    println!("\tPart 1: {}", p1);
    let p2 = part2(&contents);
    println!("\tPart 2: {}", p2);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::day4::{part1, part2};

    const DATA: &'static str = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.";

    #[test]
    fn test_sample_part1() {
        let res = part1(DATA);
        assert_eq!(res, 13)
    }

    #[test]
    fn test_sample_part2() {
        let res = part2(DATA);
        assert_eq!(res, 43)
    }
}
