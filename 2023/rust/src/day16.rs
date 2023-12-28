// Dear reader,
// This one is particularly messy.
//
// Part1 and part2 are handled fairly independently and a lot of code can be
// reduced
use std::{str::FromStr, collections::{VecDeque, HashSet, HashMap}};

static FILE_PATH: &'static str = "../data/day16.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken File");
    println!("Day 16");
    part1(&contents);
    part2(&contents);
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    fn go(&self, direction: &Direction) -> Option<Position> {
        match direction {
            Direction::Up => self.y
                .checked_sub(1)
                .map(|y| Position { x: self.x, y }),
            Direction::Down => Some(
                Position { x: self.x , y: self.y + 1 }
            ),
            Direction::Left => self.x
                .checked_sub(1)
                .map(|x| Position { x, y: self.y }),
            Direction::Right => Some(
                Position { x: self.x + 1, y: self.y }
            )
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
enum Direction {
    Left,
    Right,
    Up,
    Down
}

enum Space {
    LeftMirror,
    RightMirror,
    Vertical,
    Horizontal,
    Empty,
}

struct FromCharError(char);

impl std::fmt::Display for Space {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Space::LeftMirror => write!(f, "\\"),
            Space::RightMirror => write!(f, "/"),
            Space::Horizontal => write!(f, "-"),
            Space::Vertical => write!(f, "|"),
            Space::Empty => write!(f, " ")
        } 
    }
}

impl Space {
    fn from_char(c: &char) -> Result<Space, FromCharError> {
        match c {
            '.'  => Ok(Self::Empty),
            '/'  => Ok(Self::RightMirror),
            '\\' => Ok(Self::LeftMirror),
            '-'  => Ok(Self::Horizontal),
            '|'  => Ok(Self::Vertical),
            _    => Err(FromCharError(*c))
        }
    }

    fn up(&self) -> Vec<Direction> {
        match self {
            Space::Empty       => vec![Direction::Up],
            Space::LeftMirror  => vec![Direction::Left],
            Space::RightMirror => vec![Direction::Right],
            Space::Vertical    => vec![Direction::Up],
            Space::Horizontal  => vec![Direction::Left, Direction::Right],
        }
    }

    fn down(&self) -> Vec<Direction> {
        match self {
            Space::Empty       => vec![Direction::Down],
            Space::LeftMirror  => vec![Direction::Right],
            Space::RightMirror => vec![Direction::Left],
            Space::Vertical    => vec![Direction::Down],
            Space::Horizontal  => vec![Direction::Left, Direction::Right],
        }
    }

    fn left(&self) -> Vec<Direction> {
        match self {
            Space::Empty       => vec![Direction::Left],
            Space::LeftMirror  => vec![Direction::Up],
            Space::RightMirror => vec![Direction::Down],
            Space::Vertical    => vec![Direction::Up, Direction::Down],
            Space::Horizontal  => vec![Direction::Left],
        }
    }

    fn right(&self) -> Vec<Direction> {
        match self {
            Space::Empty       => vec![Direction::Right],
            Space::LeftMirror  => vec![Direction::Down],
            Space::RightMirror => vec![Direction::Up],
            Space::Vertical    => vec![Direction::Up, Direction::Down],
            Space::Horizontal  => vec![Direction::Right],
        }
    }

    fn apply_direction(
        &self, direction: &Direction
    ) -> Vec<Direction> {
        match direction {
            Direction::Up => self.up(),
            Direction::Down => self.down(),
            Direction::Left => self.left(),
            Direction::Right => self.right(),
        }
    }
}

struct Grid(Vec<Vec<Space>>);

impl FromStr for Grid {
    type Err = FromCharError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
            let grid = s.lines() 
                .filter(|line| !line.is_empty())
                .map(|line| {
                    line.chars()
                        .map(|c| Space::from_char(&c))
                        .collect::<Result<Vec<Space>, FromCharError>>()
                })
                .collect::<Result<Vec<Vec<Space>>, FromCharError>>()?;

        Ok(Grid(grid))
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new(); 
        for row in &self.0 {
            for col in row {
                display.push_str(
                    &format!("{col}")
                );
            }
            display.push_str("\n");
        }
        write!(f, "{display}")
    }
}

impl Grid {
    fn unsafe_get(&self, position: &Position) -> &Space {
        &self.0[position.y][position.x]
    }

    fn contains_position(&self, position: &Position) -> bool {
        self.0.len() > position.y
            && self.0.first().unwrap().len() > position.x
    }
}

struct GridTraverser {
    mirror_grid: Grid,
    energy_grid: Vec<u128>,
    position_queue: VecDeque<Position>,
    direction_queue: VecDeque<Direction>,
    history_hash: HashSet<(Position, Direction)>
}

impl GridTraverser {
    fn build(
        grid: Grid,
        position: Position,
        direction: Direction
    ) -> GridTraverser {
        let energy_grid = vec![0u128; grid.0.len()];
        let position_queue = VecDeque::new();
        let direction_queue = VecDeque::new();
        let history_hash = HashSet::new();

        let mut gt = GridTraverser {
            mirror_grid: grid,
            energy_grid,
            position_queue,
            direction_queue,
            history_hash
        };
        gt.add_position(&position, &direction);
        gt
    }

    fn charge_position(&mut self, position: &Position) {
        let new_vec = 1 << position.x;
        self.energy_grid[position.y] |= new_vec;
    }

    fn check_position(&self, position: &Position) -> bool {
        position.y < self.mirror_grid.0.len()
            && position.x < self.mirror_grid.0
            .first()
            .unwrap()
            .len()
    }

    fn apply_position_and_direction(
        &mut self,
        position: &Position,
        direction: Direction
    ) {
        let next_position = position.go(&direction);

        // End early if position is negative
        if let None = next_position {
            return;
        }

        let next_position = next_position.unwrap();

        // End early if position does not exist in grid
        if !self.check_position(&next_position) {
            return;
        }

        // End early if position has already been traversed in
        // the given direction
        if self.check_history(&next_position, &direction)
        {
            return;
        }

        self.add_position(&next_position, &direction);
    }

    fn add_position(&mut self, position: &Position, direction: &Direction) {
        self.charge_position(&position);
        self.history_hash.insert((*position, *direction));
        let directions = &self
            .mirror_grid
            .unsafe_get(&position)
            .apply_direction(&direction);

        for dir in directions {
            self.position_queue.push_back(*position);
            self.direction_queue.push_back(*dir);
        }
    }

    fn check_history(
        &self, position: &Position, direction: &Direction
    ) -> bool {
        self.history_hash.contains(&(*position, *direction))
    }

    fn traverse(&mut self) {
        while !self.position_queue.is_empty() {
            // println!("{self}");
            let position = self.position_queue
                .pop_front()
                .unwrap();
            let direction = self.direction_queue
                .pop_front()
                .unwrap();
            self.apply_position_and_direction(&position, direction)
        }
    }
}

impl std::fmt::Display for GridTraverser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new(); 
        display.push_str(
            &format!(
                "Position: {:?}, Direction: {:?}\n",
                self.position_queue, self.direction_queue
            )
        );
        for row_idx in 0..self.mirror_grid.0.len() {
            for col_idx in 0..self.mirror_grid.0
                .first()
                .unwrap()
                .len()
            {
                let space = &self.mirror_grid.0[row_idx][col_idx];
                if let Space::Empty = space {
                    let charge_check = 1 << col_idx;
                    let charge = (self.energy_grid[row_idx] & charge_check) > 0;
                    if charge {
                        display.push_str("#");
                    } else {
                        display.push_str(".");
                    }
                } else {
                    display.push_str(
                        &format!("{space}")
                    );
                }
            }
            display.push_str("\n");
        }
        write!(f, "{display}")
    }
}

#[derive(Clone)]
struct EnergyGrid(Vec<u128>);

impl EnergyGrid {
    fn build(
         length: usize, position: &Position
    ) -> EnergyGrid {
        let mut grid = vec![0u128; length];
        grid[position.y] |= 1 << position.x;
        EnergyGrid(grid)
    }

    fn combine(&self, energy_grid: &EnergyGrid) -> EnergyGrid {
        EnergyGrid(
            self.0.iter()
                .zip(energy_grid.0.iter())
                .map(|(left, right)| left | right)
                .collect::<Vec<u128>>()
        )
    }

    fn count_energized(&self) -> u32 {
        self.0
            .iter()
            .map(|vec| vec.count_ones())
            .sum()
    }
}

impl std::fmt::Display for EnergyGrid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new();
        for vec in &self.0 {
            display.push_str(
                &format!("{vec:#012b}\n").chars().rev().collect::<String>()
            );
        }
        write!(f, "{display}")
    }
}

struct MasterTraverser {
    grid: Grid,

    // Cache for completed paths
    grid_cache: HashMap<(Position, Direction), EnergyGrid>,

    // Cache for current uncompleted path
    // I'm trying a hacky way around loops by making it
    // so that a point has to be hit twice to cut the chain
    // early
    path_cache: HashMap<(Position, Direction), bool>,
}

impl MasterTraverser {
    fn build(grid: Grid) -> MasterTraverser {
        let grid_cache = HashMap::new();
        let path_cache = HashMap::new();

        MasterTraverser { grid, grid_cache, path_cache }
    }

    fn in_grid_cache(
        &self, position: &Position, direction: &Direction
    ) -> bool {
        self.grid_cache.contains_key(&(*position, *direction))
    }

    fn in_path_cache(
        &self, position: &Position, direction: &Direction
    ) -> bool {
        *self.path_cache.get(&(*position, *direction)).unwrap_or(&false)
    }

    fn add_to_path_cache(
        &mut self, position: &Position, direction: &Direction
    ) {
        let key = (*position, *direction);
        if let Some(_) = self.path_cache.get_mut(&key) {
            self.path_cache.insert(key, true);
        } else {
            self.path_cache.insert(key, false);
        }
    }

    fn from_cache(
        &self, position: &Position, direction: &Direction
    ) -> EnergyGrid {
        self.grid_cache[&(*position, *direction)].clone()
    }

    fn get_next(
        &self, position: &Position, direction: &Direction
    ) -> Option<Vec<(Position, Direction)>> {
        let directions = self.grid.unsafe_get(&position)
            .apply_direction(direction);

        let pairs: Vec<(Position, Direction)> = directions
            .into_iter()
            .filter_map(|dir| position.go(&dir).map(|pos| (pos, dir)))
            .filter(|(pos, dir)| self.grid.contains_position(pos) & !self.in_path_cache(pos, dir) )
            .collect();

        if pairs.is_empty() {
            None
        } else {
            Some(pairs)
        }
    }

    fn add_to_cache(
        &mut self, position: &Position, direction: &Direction, energy_grid: &EnergyGrid
    ) {
        self.grid_cache.insert((*position, *direction), energy_grid.clone());
    }

    fn traverse_from_start(
        &mut self,
        position: &Position,
        direction: &Direction
    ) {
        self.traverse(position, direction);
        self.path_cache.clear();
    }

    // Recursive function to traverse the grid
    // and store a grid per position/ direction pairings
    fn traverse(
        &mut self,
        position: &Position,
        direction: &Direction
    ) -> EnergyGrid {
        // If position direction in cache, return value
        if self.in_grid_cache(position, direction) {
            self.from_cache(position, direction)

        // Recursively traverse and return new energy grid
        } else {
            let energy_grid = EnergyGrid::build(self.grid.0.len(), position);
            println!("{}", energy_grid);

            self.add_to_path_cache(position, direction);

            // If there is a next step to be taken, generate sub_grid
            // and combine it with current grid
            if let Some(pairs) = self.get_next(position, direction) {
                let sub_grid = pairs.iter()
                    .map(|(pos, dir)| self.traverse(pos, dir))
                    .reduce(|left, right| left.combine(&right))
                    .unwrap();
                let energy_grid = energy_grid.combine(&sub_grid);
                self.add_to_cache(position, direction, &energy_grid);
                energy_grid

            // Return grid if no more progress can be made
            } else {
                energy_grid
            }
        }
    }
}


//    
//  /<<<\
//  v   |<<
// <->>>/
//  ^

fn get_end_points(length: usize, width: usize) -> Vec<(Position, Direction)> {
    let mut vec = Vec::new();
    for w in 0..width {
        let entry = (Position { x: w, y: 0 }, Direction::Down);
        vec.push(entry);
        let entry = (Position { x: w, y: (length -1) }, Direction::Up);
        vec.push(entry);
    }
    for l in 0..length {
        let entry = (Position { x: 0, y: l }, Direction::Right);
        vec.push(entry);
        let entry = (Position { x: (width - 1), y: l }, Direction::Left);
        vec.push(entry);
    }

    vec
}

fn part1(contents: &str) -> u32 {
    let grid = contents.parse::<Grid>().ok().unwrap();
    let starting_point = Position { x: 0, y: 0};
    let starting_direction = Direction::Right;

    let mut traverser = GridTraverser::build(
        grid, starting_point, starting_direction
    );
    traverser.traverse();
    let answer = traverser
        .energy_grid
        .iter()
        .map(|vec| vec.count_ones())
        .sum();
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u32 {
    let grid = contents.parse::<Grid>().ok().unwrap();
    let endpoints = get_end_points(
        grid.0.len(), grid.0.first().unwrap().len()
    );
    let mut master = MasterTraverser::build(grid);

    for (position, direction) in &endpoints {
        master.traverse_from_start(
            &position, &direction
        );
    }

    println!("{}", master.grid_cache[&(Position{ x: 3, y: 0}, Direction::Down)]);
    println!("{}", master.grid_cache[&(Position{ x: 1, y: 0}, Direction::Left)]);

    let answer = master
        .grid_cache
        .into_iter()
        .filter(|(point, _)| endpoints.contains(point))
        .map(|(_, v)| v.count_energized())
        .max()
        .unwrap();

    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = r#"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"#;

#[test]
fn test_part1() {
    let answer = part1(DATA);
    assert_eq!(answer, 46);
}

#[test]
fn test_part2() {
    let answer = part2(DATA);
    assert_eq!(answer, 51);
}

}
    
