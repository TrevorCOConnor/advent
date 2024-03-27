use std::{collections::HashSet, hash::Hash};
use priority_queue::PriorityQueue;
use rust::{Direction, Position, Matrix};

static FILE_PATH: &'static str = "../data/day17.txt";

type Steps = u32;

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 17");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let matrix = create_matrix(contents);
    let mut grid_traverser = GridTraverser::build(matrix, false);
    let answer = grid_traverser.find_min_heat_loss();
    println!("Part 1: {}", answer);
    answer
    // println!("Part 1: TODO");
    // 0
}

fn part2(contents: &str) -> u32 {
    let matrix = create_matrix(contents);
    let mut grid_traverser = GridTraverser::build(matrix, true);
    let answer = grid_traverser.find_min_heat_loss();
    println!("Part 2: {}", answer);
    // println!("Part 2: TODO");
    answer
}

fn create_matrix(text: &str) -> Matrix<u32> {
    let matrix = text.lines()
        .filter(|line| !line.is_empty())
        .map(|line| line
            .chars()
            .map(|c|
                c.to_string()
                .parse::<u32>()
                .expect("Non digit character found")
            )
            .collect()
        )
        .collect::<Vec<Vec<u32>>>();
    Matrix::build(matrix)
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
struct PositionState {
    position: Position,
    direction: Direction,
    steps: Steps
}

impl PositionState {
    // Branch Position state, discontinuing paths that would exceed three
    // steps in one direction
    fn normal_branch(&self) -> Vec<PositionState> {
        let (dir_a, dir_b) = self.direction.perpendicular();

        [
            (self.direction, self.steps+1),
            (dir_b, 1),
            (dir_a, 1)
        ]
        .into_iter()
        .filter_map(|(d, s)| {
            let new_pos = self.position.go(&d)?;
            if s <= 3 {
                Some(
                    PositionState {
                        position: new_pos,
                        direction: d,
                        steps: s
                    }
                )
            } else { None }
        })
        .collect()
    }

    // Take a step in the same direction
    fn step(&self) -> Option<PositionState> {
        let new = self.position.go(&self.direction)?;
        Some(
            PositionState {
                position: new,
                direction: self.direction,
                steps: self.steps + 1
            }
        )
    }

    // Branch PositionState based on ultra logic
    fn ultra_branch(&self) -> Vec<PositionState> {
        if self.steps < 4 {
            if let Some(pos_state) = self.step() {
                vec![pos_state]
            } else {
                Vec::new()
            }
        } else {
            let (dir_a, dir_b) = self.direction.perpendicular();
            [
                (self.direction, self.steps+1),
                (dir_b, 1),
                (dir_a, 1)
            ]
            .into_iter()
            .filter_map(|(d, s)| {
                let new_pos = self.position.go(&d)?;
                if s <= 10 {
                    Some(
                        PositionState {
                            position: new_pos,
                            direction: d,
                            steps: s
                        }
                    )
                } else { None }
            })
            .collect()
        }
    }

    // Generic branch function for easier api
    fn branch(&self, ultra: bool) -> Vec<PositionState> {
        if ultra {
            self.ultra_branch()
        } else {
            self.normal_branch()
        }
    }

    // witchcraft
    // turns out not that helpful
    fn my_hash(&self) -> u32 {
        // trevor's
        (self.position.x as u32) * 100000
        + (self.position.y as u32) * 100
        + (self.steps + 1) * 10
        + (self.direction as u32)
    }
}


#[derive(Debug, Eq, PartialEq, Hash)]
struct Path {
    heat: u32,
    position_state: PositionState
}

struct GridTraverser {
    grid: Matrix<u32>,
    traverse_cache: HashSet<u32>,
    is_ultra: bool,
}

impl GridTraverser {
    fn build(matrix: Matrix<u32>, is_ultra: bool) -> GridTraverser {
        let traverse_cache = HashSet::new();
        GridTraverser {
            grid: matrix,
            traverse_cache,
            is_ultra,
        }
    }

    fn rank_path(&self, path: &Path) -> u32 {
        u32::MAX - (
            path.heat
            + (self.grid.cols() - (path.position_state.position.x + 1)) as u32
            + (self.grid.rows() - (path.position_state.position.y + 1)) as u32
        )
    }

    fn find_min_heat_loss(&mut self) -> u32 {
        let right_path = Path {
            heat: 0,
            position_state: PositionState {
                position: Position { x: 0, y: 0 },
                direction: Direction::Right,
                steps: 0
            }
        };

        let down_path = Path {
            heat: 0,
            position_state: PositionState {
                position: Position { x: 0, y: 0 },
                direction: Direction::Down,
                steps: 0
            }
        };

        self.traverse_cache.insert(right_path.position_state.my_hash());
        self.traverse_cache.insert(down_path.position_state.my_hash());

        let mut queue: PriorityQueue<Path, u32> = PriorityQueue::from_iter([
            (right_path, u32::MAX),
            (down_path, u32::MAX)
        ]);

        let end_position = Position {
            x: self.grid.cols() - 1,
            y: self.grid.rows() - 1
        };

        while !queue.is_empty() {
            let next = queue.pop().unwrap().0;            

            // Check for end
            if next.position_state.position == end_position {
                // A bit hacky
                // Cannot end if less than 4 steps
                if self.is_ultra {
                    if next.position_state.steps < 4 {
                        continue
                    }
                }

                return next.heat
            }

            let branches = self.step_path(&next);
            for branch in branches {
                let priority = self.rank_path(&branch);
                queue.push(branch, priority);
            }
        }

        return 0
    }

    fn step_path(&mut self, path: &Path) -> Vec<Path> {
        let branches = path.position_state.branch(self.is_ultra);
        let mut new_paths: Vec<Path> = Vec::new();

        for branch in branches {
            // Check if new position is in the grid
            if let Some(added_heat) = self.grid.get(&branch.position) {
               // Check that the new heat is less than the previous heat when
               // this position state was reached (if that has happened yet)
               let branch_hash = branch.my_hash();
               if !self.traverse_cache.contains(&branch_hash){
                    self.traverse_cache.insert(branch_hash);
                    let new_path = Path {heat: (path.heat + added_heat), position_state: branch };
                    new_paths.push(new_path);
               }
            }
        }

        new_paths
    }
}


#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
";

    static DATA2: &'static str = "
111111111111
999999999991
999999999991
999999999991
999999999991
";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 102);
    }

    // #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 94);

        let answer = part2(DATA2);
        assert_eq!(answer, 71);
    }

}
    
