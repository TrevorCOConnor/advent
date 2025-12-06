use std::slice::IterMut;

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

#[derive(Clone)]
pub struct Matrix<T> {
    grid: Vec<Vec<T>>,
    cols: usize,
    rows: usize,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Position {
    pub x: usize,
    pub y: usize,
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

pub struct MatrixValue<T> {
    pub value: T,
    pub pos: Position,
}

pub struct MutMatrixValue<'a, T> {
    pub value: &'a mut T,
    pub pos: Position,
}

pub struct MatrixValueIter<'a, T> {
    matrix: &'a Matrix<T>,
    index: Position,
}

impl<'a, T> Iterator for MatrixValueIter<'a, T> {
    type Item = MatrixValue<&'a T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index.x >= self.matrix.rows {
            self.index.y += 1;
            self.index.x = 0;
        }
        if self.index.y >= self.matrix.cols {
            return None;
        }

        let val = Some(MatrixValue {
            value: self.matrix.get(&self.index).unwrap(),
            pos: self.index,
        });
        self.index.x += 1;
        val
    }
}

pub struct MatrixMutValueIter<'a, T> {
    iter: IterMut<'a, MatrixValue<T>>,
}

impl<'a, T> Iterator for MatrixMutValueIter<'a, T> {
    type Item = &'a mut MatrixValue<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct ValueNeighborIter<'a, T> {
    matrix: &'a Matrix<T>,
    pos: &'a Position,
    dirs: Vec<Direction>,
}

impl<'a, T> Iterator for ValueNeighborIter<'a, T> {
    type Item = MatrixValue<&'a T>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next_dir = self.dirs.pop()?;
            let new_pos = self.pos.apply_dir(&next_dir);
            if let Some(np) = new_pos {
                let neighbor = self.matrix.get(&np);
                if let Some(v) = neighbor {
                    return Some(MatrixValue { value: v, pos: np });
                }
            }
        }
    }
}

impl<T> Matrix<T> {
    pub fn new(grid: Vec<Vec<T>>) -> Self {
        let rows = grid.len();
        let cols = grid.first().map(|row| row.len()).unwrap_or(0);
        Matrix { grid, rows, cols }
    }

    pub fn set(&mut self, pos: &Position, val: T) -> Option<()> {
        if pos.y >= self.rows {
            return None;
        }
        let row = &mut self.grid[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        row[pos.x] = val;
        Some(())
    }

    pub fn get(&self, pos: &Position) -> Option<&T> {
        if pos.y >= self.rows {
            return None;
        }
        let row = &self.grid[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        Some(&row[pos.x])
    }

    pub fn get_mut(&mut self, pos: &Position) -> Option<&mut T> {
        if pos.y >= self.grid.len() {
            return None;
        }
        let row = &mut self.grid[pos.y];
        if pos.x >= row.len() {
            return None;
        }
        Some(&mut row[pos.x])
    }

    pub fn values_iter<'a>(&'a self) -> MatrixValueIter<'a, T> {
        MatrixValueIter {
            matrix: self,
            index: Position { x: 0, y: 0 },
        }
    }

    pub fn value_neighbors_iter<'a>(&'a self, pos: &'a Position) -> ValueNeighborIter<'a, T> {
        ValueNeighborIter {
            matrix: &self,
            pos,
            dirs: Direction::all(),
        }
    }
}
