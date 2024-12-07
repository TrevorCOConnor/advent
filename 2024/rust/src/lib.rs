use std::fmt::Display;

#[derive(Clone)]
pub struct Matrix<T>(Vec<Vec<T>>);

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

impl Direction {
    pub fn all() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::NorthEast,
            Direction::East,
            Direction::SouthEast,
            Direction::South,
            Direction::SouthWest,
            Direction::West,
            Direction::NorthWest,
        ]
    }

    pub fn rotate_90(&self) -> Direction {
        match self {
            &Direction::North => Direction::East,
            &Direction::NorthEast => Direction::SouthEast,
            &Direction::East => Direction::South,
            &Direction::SouthEast => Direction::SouthWest,
            &Direction::South => Direction::West,
            &Direction::SouthWest => Direction::NorthWest,
            &Direction::West => Direction::North,
            &Direction::NorthWest => Direction::NorthEast,
        }
    }

    pub fn rev(&self) -> Direction {
        match self {
            &Direction::North => Direction::South,
            &Direction::NorthEast => Direction::SouthWest,
            &Direction::East => Direction::South,
            &Direction::SouthEast => Direction::NorthWest,
            &Direction::South => Direction::North,
            &Direction::SouthWest => Direction::NorthEast,
            &Direction::West => Direction::East,
            &Direction::NorthWest => Direction::SouthEast,
        }
    }
}

impl Position {
    pub fn apply_dir(&self, dir: &Direction) -> Option<Position> {
        match dir {
            Direction::North => Some(Position {
                x: self.x,
                y: self.y + 1,
            }),
            Direction::NorthEast => Some(Position {
                x: self.x + 1,
                y: self.y + 1,
            }),
            Direction::East => Some(Position {
                x: self.x + 1,
                y: self.y,
            }),
            Direction::SouthEast => {
                let new_y = self.y.checked_sub(1)?;
                Some(Position {
                    x: self.x + 1,
                    y: new_y,
                })
            }
            Direction::South => {
                let new_y = self.y.checked_sub(1)?;
                Some(Position {
                    x: self.x,
                    y: new_y,
                })
            }
            Direction::SouthWest => {
                let new_y = self.y.checked_sub(1)?;
                let new_x = self.x.checked_sub(1)?;
                Some(Position { x: new_x, y: new_y })
            }
            Direction::West => {
                let new_x = self.x.checked_sub(1)?;
                Some(Position {
                    x: new_x,
                    y: self.y,
                })
            }
            Direction::NorthWest => {
                let new_x = self.x.checked_sub(1)?;
                Some(Position {
                    x: new_x,
                    y: self.y + 1,
                })
            }
        }
    }
}

impl<T> Matrix<T> {
    pub fn new(rows: Vec<Vec<T>>) -> Self {
        Matrix(rows)
    }

    pub fn values(&self) -> Vec<(Position, &T)> {
        self.0
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, val)| (Position { x, y }, val))
                    .collect::<Vec<(Position, &T)>>()
            })
            .flatten()
            .collect()
    }

    pub fn get(&self, pos: &Position) -> Option<&T> {
        if pos.y >= self.0.len() {
            return None;
        }
        let row = &self.0[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        Some(&row[pos.x])
    }

    pub fn get_mut(&mut self, pos: &Position) -> Option<&mut T> {
        if pos.y >= self.0.len() {
            return None;
        }
        let row = &mut self.0[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        Some(&mut row[pos.x])
    }

    pub fn set(&mut self, pos: &Position, val: T) -> Option<()> {
        if pos.y >= self.0.len() {
            return None;
        }
        let row = &mut self.0[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        row[pos.x] = val;
        Some(())
    }
}

impl<T: Clone> Matrix<T> {
    pub fn cloned_values(&self) -> Vec<(Position, T)> {
        self.0
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, val)| (Position { x, y }, val.clone()))
                    .collect::<Vec<(Position, T)>>()
            })
            .flatten()
            .collect()
    }
}

impl<T: Display> Display for Matrix<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut disp = String::new();
        for row in self.0.iter().rev() {
            for value in row {
                disp.push_str(&format!("{}", value));
            }
            disp.push_str("\n");
        }

        write!(f, "{}", disp)
    }
}
