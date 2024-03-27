#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right
}

impl Direction {
    pub fn all() -> Vec<Direction> {
        vec![
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
    }

    pub fn perpendicular(&self) -> (Direction, Direction) {
        if *self == Direction::Up || *self == Direction::Down {
            (Direction::Left, Direction::Right)
        } else {
            (Direction::Up, Direction::Down)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Position {
    pub x: usize,
    pub y: usize
}

impl Position {
    pub fn go(&self, direction: &Direction) -> Option<Position> {
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

    pub fn adjacent_positions(&self) -> Vec<Position> {
        [self.go(&Direction::Up),
        self.go(&Direction::Down),
        self.go(&Direction::Left),
        self.go(&Direction::Right)]
            .into_iter()
            .flatten()
            .collect()
    }
}


pub struct Matrix<A> {
    matrix: Vec<Vec<A>>
}

impl<A: std::fmt::Debug> Matrix<A> {
    pub fn get(&self, pos: &Position) -> Option<&A> {
        self.matrix.get(pos.y)?.get(pos.x)
    }

    pub fn build(matrix: Vec<Vec<A>>) -> Matrix<A> {
        Matrix { matrix }
    }

    pub fn rows(&self) -> usize {
        self.matrix.len()
    }

    pub fn cols(&self) -> usize {
        self.matrix
            .get(0)
            .map(|r| r.len())
            .unwrap_or(0)
    }
}
