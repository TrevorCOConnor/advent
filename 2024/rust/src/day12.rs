use std::collections::{HashSet, VecDeque};

use rust::{Direction, Matrix, Position};

static FILE_PATH: &'static str = "../data/day12.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 12");
    part1(&contents);
    part2(&contents);
}

fn read_data(contents: &str) -> Matrix<char> {
    Matrix::new(
        contents
            .lines()
            .rev()
            .map(|line| line.chars().collect())
            .collect(),
    )
}

type Edge = (usize, usize);

fn generate_edges(pos: &Position) -> Vec<Edge> {
    let Position { x, y } = *pos;
    let x = 2 * (x + 1);
    let y = 2 * (y + 1);
    vec![(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
}

struct GroupFinder {
    edges: HashSet<Edge>,
    count: usize,
    seen: HashSet<Position>,
    queue: VecDeque<Position>,
}

impl GroupFinder {
    fn new() -> Self {
        GroupFinder {
            edges: HashSet::new(),
            count: 0,
            seen: HashSet::new(),
            queue: VecDeque::new(),
        }
    }

    fn calculate(char_matrix: &Matrix<char>, starting_pos: &Position, discount: bool) -> usize {
        let mut queue = VecDeque::new();
        let mut seen = HashSet::new();
        let mut sum = 0;
        queue.push_front(*starting_pos);
        loop {
            if queue.is_empty() {
                break;
            }

            let pos = queue.pop_front().unwrap();
            if seen.contains(&pos) {
                continue;
            }
            seen.insert(pos);

            let group = char_matrix.get(&pos).unwrap();
            let mut gf = GroupFinder::new();
            gf.seen.extend(seen.clone());
            gf.count += 1;
            gf.edges.extend(generate_edges(&pos));
            gf.find_group(char_matrix, group, &pos);

            if discount {
                sum += gf.count * gf.count_sides()
            } else {
                sum += gf.edges.len() * gf.count;
            }
            queue.extend(gf.queue);
            seen.extend(gf.seen);
        }
        sum
    }

    fn find_group(&mut self, char_matrix: &Matrix<char>, group: &char, pos: &Position) {
        let next_positions = [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
        .filter_map(|dir| pos.apply_dir(dir))
        .filter(|pos| char_matrix.get(pos).is_some());

        for next_pos in next_positions {
            if !self.seen.contains(&next_pos) {
                // Merge-able
                if char_matrix.get(&next_pos).unwrap() == group {
                    let edges = generate_edges(&next_pos);
                    for edge in edges {
                        if self.edges.contains(&edge) {
                            self.edges.remove(&edge);
                        } else {
                            self.edges.insert(edge);
                        }
                    }
                    self.count += 1;
                    self.seen.insert(next_pos);
                    self.find_group(char_matrix, group, &next_pos)
                } else {
                    self.queue.push_back(next_pos);
                }
            }
        }
    }

    // Same as counting corners
    // This is a HOT mess.
    // In case I'm inspired to clean it up later:
    // We are looking at all horizontal edges and checking all vertical edges:
    // there are 4 possible edges. If any two share the same x value, then one of them is not a
    // real corner. Example below:
    // AAAA
    // AAEA
    // AEAA
    // AAAA
    fn count_sides(&self) -> usize {
        let edge_vec = self.edges.iter().collect::<Vec<&Edge>>();
        let mut edge_count = 0;
        for edge in edge_vec.iter() {
            if edge.0 % 2 == 0 {
                let matching = edge_vec
                    .iter()
                    .filter(|other_edge| {
                        edge.0.abs_diff(other_edge.0) == 1 && edge.1.abs_diff(other_edge.1) == 1
                    })
                    .collect::<Vec<&&Edge>>();
                if matching.len() == 1 {
                    edge_count += 1
                } else if matching.len() == 2 {
                    if matching[0].0 == matching[1].0 {
                        edge_count += 1
                    } else {
                        edge_count += 2
                    }
                } else if matching.len() > 2 {
                    edge_count += 2
                }
            }
        }
        edge_count
    }
}

fn part1(contents: &str) -> usize {
    let matrix = read_data(contents);
    let answer = GroupFinder::calculate(&matrix, &Position { x: 0, y: 0 }, false);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let matrix = read_data(contents);
    let answer = GroupFinder::calculate(&matrix, &Position { x: 0, y: 0 }, true);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE";

    static DATA2: &'static str = "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA";

    static DATA3: &'static str = "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO";

    static DATA4: &'static str = "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE";

    static DATA5: &'static str = "AAAA
BBCD
BBCC
EEEC";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 1930);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 1206);
    }

    #[test]
    fn test_bug() {
        let answer = part2(DATA2);
        assert_eq!(answer, 368);
    }

    #[test]
    fn test_bug2() {
        let answer = part2(DATA3);
        assert_eq!(answer, 436);
    }

    #[test]
    fn test_bug3() {
        let answer = part2(DATA4);
        assert_eq!(answer, 236);
    }

    #[test]
    fn test_bug4() {
        let answer = part2(DATA5);
        assert_eq!(answer, 80);
    }
}
