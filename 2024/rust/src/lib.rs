pub struct Matrix<T>(Vec<Vec<T>>);

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub x: usize,
    pub y: usize
}

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest
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
            Direction::NorthWest
       ]
   }
}

impl Position {
    pub fn apply_dir(&self, dir: &Direction) -> Option<Position> {
        match dir  {
            Direction::North => {
                Some(Position{x: self.x, y: self.y + 1})
            },
            Direction::NorthEast => {
                Some(Position{x: self.x + 1, y: self.y + 1})
            },
            Direction::East => {
                Some(Position{x: self.x + 1, y: self.y})
            },
            Direction::SouthEast => {
                let new_y = self.y.checked_sub(1)?;
                Some(Position{x: self.x + 1, y: new_y})
            },
            Direction::South => {
                let new_y = self.y.checked_sub(1)?;
                Some(Position{x: self.x, y: new_y})
            },
            Direction::SouthWest => {
                let new_y = self.y.checked_sub(1)?;
                let new_x = self.x.checked_sub(1)?;
                Some(Position{x: new_x, y: new_y})
            },
            Direction::West => {
                let new_x = self.x.checked_sub(1)?;
                Some(Position{x: new_x, y: self.y})
            },
            Direction::NorthWest => {
                let new_x = self.x.checked_sub(1)?;
                Some(Position{x: new_x, y: self.y + 1})
            },
        } 
    }
}

impl<T> Matrix<T> {
    pub fn new(rows: Vec<Vec<T>>) -> Self {
        Matrix(rows)
    }

    pub fn values(&self) -> Vec<(Position, &T)> {
        self.0.iter()
            .enumerate()
            .map(|(y, row)|
                row.iter()
                    .enumerate()
                    .map(|(x, val)| (Position{x, y}, val))
                    .collect::<Vec<(Position, &T)>>()
            ).flatten()
            .collect()
    }

    pub fn get(&self, pos: &Position) -> Option<&T> {
        if pos.y >= self.0.len() {
            return None
        } 
        let row = &self.0[pos.y];
        if pos.x >= row.len() {
            return None
        }
        Some(&row[pos.x])
    }
}
