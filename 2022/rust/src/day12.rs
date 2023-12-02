use bit_vec::BitVec;

struct Point{
    x: u32,
    y: u32
}


struct History {
    x_max: usize,
    y_max: usize,
    vec: BitVec
}

impl History {
    fn contains(&self, point: Point) -> bool {
        self.vec.and(
            point.x + (self.x_max * point.y)
        )
    }
}


enum PathTree {
    Stem(Point),
    Branch(Point, Point, Option<Point>, Option<Point>)
}
