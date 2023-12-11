use std::str::FromStr;

static FILE_PATH: &'static str = "../data/day10.txt";

#[derive(Clone, Copy, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Pipe {
    Start,
    Horizontal,
    Vertical,
    LeftUp,
    LeftDown,
    RightUp,
    RightDown
}

impl Pipe {
    fn from_char(c: char) -> Option<Pipe> {
        match c {
            'S' => Some(Pipe::Start),
            '-' => Some(Pipe::Horizontal),
            '|' => Some(Pipe::Vertical),
            'L' => Some(Pipe::LeftUp),
            'F' => Some(Pipe::LeftDown),
            'J' => Some(Pipe::RightUp),
            '7' => Some(Pipe::RightDown),
              _ => None
        }
    }

    fn left(&self) -> Option<Direction> {
        match self {
            Pipe::Horizontal => Some(Direction::Left),
            Pipe::LeftUp => Some(Direction::Up),
            Pipe::LeftDown => Some(Direction::Down),
            _ => None,
        }
    }

    fn right(&self) -> Option<Direction> {
        match self {
            Pipe::Horizontal => Some(Direction::Right),
            Pipe::RightUp => Some(Direction::Up),
            Pipe::RightDown => Some(Direction::Down),
            _ => None,
        }
    }

    fn up(&self) -> Option<Direction> {
        match self {
            Pipe::Vertical => Some(Direction::Up),
            Pipe::LeftDown => Some(Direction::Right),
            Pipe::RightDown => Some(Direction::Left),
            _ => None,
        }
    }

    fn down(&self) -> Option<Direction> {
        match self {
            Pipe::Vertical => Some(Direction::Down),
            Pipe::LeftUp => Some(Direction::Right),
            Pipe::RightUp => Some(Direction::Left),
            _ => None,
        }
    }

    fn through(&self, direction: &Direction) -> Option<Direction> {
        match direction {
            Direction::Up => self.up(),
            Direction::Down => self.down(),
            Direction::Right => self.right(),
            Direction::Left => self.left(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Position { x: usize, y: usize }

impl Position {
    fn up(&self) -> Option<Position> {
        self.y.checked_sub(1)
            .map(|new_y| Position { x: self.x, y: new_y })
    }

    fn down(&self) -> Option<Position> {
        Some(Position { x: self.x, y: self.y + 1 })
    }

    fn left(&self) -> Option<Position> {
        self.x.checked_sub(1)
            .map(|new_x| Position { x: new_x, y: self.y })
    }

    fn right(&self) -> Option<Position> {
        Some(Position { x: self.x + 1, y: self.y })
    }

    fn get_adjacent_positions(&self) ->
        Vec<(Direction, Position)>
    {
        [
            (Direction::Up, self.up()),
            (Direction::Down, self.down()),
            (Direction::Left, self.left()),
            (Direction::Right, self.right()),
        ].iter()
        .filter_map(|(dir, pos)| pos.as_ref().map(|p| (*dir, *p)))
        .collect()
    }

    fn try_to(&self, direction: &Direction) -> Option<Position> {
        match direction {
            Direction::Up => self.up(),
            Direction::Down => self.down(),
            Direction::Left => self.left(),
            Direction::Right => self.right(),
        }
    }

    fn to(&self, direction: &Direction) -> Position {
        self.try_to(direction).expect("Position does not exist")
    }

    fn from_idxs(x: usize, y: usize) -> Position {
        Position { x, y }
    }
}


#[derive(Debug)]
struct Grid<T>(Vec<Vec<T>>);

impl<T> Grid<T> {
    fn get(&self, position: &Position) -> Option<&T> {
        self.0.get(position.y)
            .and_then(|vec| vec.get(position.x))
    }

    fn set(&mut self, position: &Position, value: T) {
        self.0[position.y][position.x] = value
    }
}

impl Grid<Option<Pipe>> {
    fn try_move_through(&self, position: &Position, direction: &Direction)
        -> Option<Direction>
    {
        self.get(&position)
            .and_then( |entry|
                entry.as_ref().and_then(|pipe| pipe.through(direction))
            )
    }

    fn move_through(&self, position: &Position, direction: &Direction)
        -> Direction
    {
        let entry = self.get(&position).expect(
            &format!("Position {position:?} does not exist in grid")
        );
        let pipe = entry.as_ref().expect("Pipe does not exist");
        pipe.through(direction).expect(
            &format!("Pipe {pipe:?} at {position:?} does not support direction {direction:?}")
        )
    }

    // Messy, should clean up
    fn find_start(&self) -> Position {
        self.0.iter()
            .enumerate()
            .find_map(|(y, vec)| {
                vec
                    .iter()
                    .enumerate()
                    .find_map(|(x, pipe)| pipe.as_ref().and_then(|p| if p == &Pipe::Start { Some(x) } else { None} )) 
                    .map(|x| Position { x, y})
            }).expect("Could not find starting pipe")
    }

    fn find_first_direction(&self, start: &Position) -> Direction {
        *start
            .get_adjacent_positions()
            .iter()
            .find_map(|(dir, pos)|
                if self.try_move_through(pos, dir).is_some() {
                    Some(dir)
                } else { None }
            )
            .expect("Could not find first pipe")
    }

    fn find_loop_length(&self) -> u32 {
        let mut current = self.find_start();
        let mut direction = self.find_first_direction(&current);

        let mut steps = 0;

        loop {
            let new_pos = current.to(&direction);
            if self.get(&new_pos).unwrap().as_ref().unwrap() == &Pipe::Start {
                break;
            }
            steps += 1;
            let new_dir = self.move_through(&new_pos, &direction);
            current = new_pos;
            direction = new_dir;
        }

        steps
    }

    // Expand grid to ensure spaces between pipes
    fn expand(self) -> Grid<Option<Pipe>> {
        let mut expanded = Vec::with_capacity(self.0.len() * 2);
        let row_length = self.0.first().unwrap().len();
        let num_rows = self.0.len();

        for row_idx in 0..num_rows {
            let mut expanded_row = Vec::new();
            let mut new_row = Vec::new(); 
            for col_idx in 0..row_length {
                let orig_value = self.get(&Position::from_idxs(col_idx, row_idx)).unwrap();
                expanded_row.push(orig_value.clone());

                let below_value = self.get(&Position::from_idxs(col_idx, row_idx + 1)).unwrap_or(&None);

                // Duplicate Vertical pipes vertically
                if orig_value.as_ref().is_some_and(|pipe| pipe.up().is_some())
                    || below_value.as_ref().is_some_and(|pipe| pipe.down().is_some())
                {
                    new_row.push(Some(Pipe::Vertical));
                } else {
                    new_row.push(None);
                }
                new_row.push(None);

                let right_value = self.get(
                    &Position::from_idxs(col_idx + 1, row_idx)
                ).unwrap_or(&None);

                // Duplicate Horizontal pipes horizontally
                // Need to check current and next value to prevent gaps
                if orig_value.as_ref().is_some_and(|pipe| pipe.left().is_some())
                    || right_value.as_ref().is_some_and(|pipe| pipe.right().is_some())
                {
                    expanded_row.push(Some(Pipe::Horizontal));
                } else {
                    expanded_row.push(None);
                }
            }
            expanded.push(expanded_row);
            expanded.push(new_row);
        };
        Grid(expanded)
    }

    fn count_internal_positions(self) -> usize {
        let mut expanded_oriented = self.reduce_to_loop()
            .expand()
            .convert_to_orientation();

        for position in expanded_oriented.edge_positions().iter() {
            expanded_oriented.update_adjacent_to_external(position)
        }

        let collapsed: Grid<Orientation> = expanded_oriented.collapse();
        collapsed.0
            .iter()
            .map(|vec| vec.iter().filter(|v| v.is_internal()).count())
            .sum()
    }

    fn reduce_to_loop(&self) -> Grid<Option<Pipe>> {
        let row_length = self.0.first().unwrap().len();
        let mut grid: Grid<Option<Pipe>> = Grid(vec![vec![None; row_length]; self.0.len()]); 

        let mut current = self.find_start();
        let mut direction = self.find_first_direction(&current);

        grid.set(&current, Some(Pipe::Start));

        loop {
            let new_pos = current.to(&direction);
            let maybe_pipe = self.get(&new_pos);
            if maybe_pipe.unwrap().as_ref().unwrap() == &Pipe::Start {
                break;
            }
            let new_dir = self.move_through(&new_pos, &direction);
            grid.set(&new_pos, maybe_pipe.unwrap().clone());
            current = new_pos;
            direction = new_dir;
        }
        
        grid
    }

    fn convert_to_orientation(self) -> Grid<Orientation> {
        Grid(
            self.0.into_iter()
            .map(|vec| {
                vec.into_iter()
                    .map(|mp| if mp.is_some() {Orientation::Border} else {Orientation::Internal})
                    .collect()
            })
            .collect()
        )
    }
}

// Formal Parse! No error handling implemented at the moment, though
impl FromStr for Grid<Option<Pipe>> {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(
            Grid(
                s.lines()
                    .filter(|l| !l.is_empty())
                    .map(|l| l.chars().map(|c| Pipe::from_char(c)).collect())
                    .collect()
            )
        )
    }
}

// Part2
#[derive(Clone, Copy)]
enum Orientation {
    Internal,
    External,
    Border,
}

impl Orientation {
    fn is_internal(&self) -> bool {
        match self {
            Orientation::Internal => true,
            _ => false
        }
    }

    fn is_external(&self) -> bool {
        match self {
            Orientation::External => true,
            _ => false
        }
    }

    fn is_border(&self) -> bool {
        match self {
            Orientation::Border => true,
            _ => false
        }
    }
}

impl Grid<Orientation> {
    fn collapse(self) -> Grid<Orientation> {
        Grid(
            self.0
                .into_iter()
                .map(|v| v.into_iter()
                    .step_by(2)
                    .collect()
                )
                .step_by(2)
                .collect()
        )
    }

    fn edge_positions(&self) -> Vec<Position> {
        let num_rows = self.0.len();
        let row_length = self.0.first().unwrap().len();

        let row_edges = [0, num_rows - 1].into_iter()
            .flat_map(|y| {
                (0..row_length)
                    .into_iter()
                    .map(|x| Position { x, y })
                    .collect::<Vec<Position>>()
            });
        let col_edges = [0, row_length - 1]
            .into_iter()
            .flat_map(|x| (0..num_rows)
                .into_iter()
                .map(|y| Position { x: x, y })
                .filter( |pos|
                    !(
                        pos.x == 0 && pos.y == 0
                        || pos.x == (row_length - 1) && pos.y == (num_rows - 1)
                    )
                )
                .collect::<Vec<Position>>()
            );

        row_edges.chain(col_edges).collect()
    }

    fn update_adjacent_to_external(&mut self, position: &Position) {
        if self.get(position).map(|o| o.is_external() || o.is_border()).unwrap_or(true) {
            return;
        }

        self.set(position, Orientation::External);
        for (_, adj_pos) in position.get_adjacent_positions().iter() {
            self.update_adjacent_to_external(adj_pos)
        }
    }
}

pub fn solutions() {
    let contents = std::fs::read_to_string(
        FILE_PATH
    ).expect("Broken file");
    println!("Day 10");
    part1(&contents);
    part2(&contents);
}

fn part1(contents: &str) -> u32 {
    let grid: Grid<Option<Pipe>> = contents.parse().expect("Should parse to grid");
    let length = grid.find_loop_length();
    let answer = {
        if length.rem_euclid(2) == 0 {
            length.div_euclid(2)
        } else {
            length.div_euclid(2) + 1
        }
    };
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> usize {
    let grid: Grid<Option<Pipe>> = contents.parse().expect("Should parse to grid");
    let answer = grid.count_internal_positions();
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static FIRST_DATA: &'static str = "
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
";

    static SECOND_DATA: &'static str = "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
";

    static THIRD_DATA: &'static str = "
..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........
";

    static FOURTH_DATA: &'static str = "
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
";

    static FIFTH_DATA: &'static str = "
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
";

    #[test]
    fn test_part1() {
        let first_answer = part1(FIRST_DATA);
        assert_eq!(first_answer, 4);

        let second_answer = part1(SECOND_DATA);
        assert_eq!(second_answer, 8);
    }

    #[test]
    fn test_part2() {
        let first_answer = part2(THIRD_DATA);
        assert_eq!(first_answer, 4);

        let second_answer = part2(FOURTH_DATA);
        assert_eq!(second_answer, 8);

        let third_answer = part2(FIFTH_DATA);
        assert_eq!(third_answer, 10);
    }
}
