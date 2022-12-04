use std::str::FromStr;

static FILE_PATH: &'static str = "../data/Day2.txt";
static WIN      : u32 = 6;
static TIE      : u32 = 3;
static LOSS     : u32 = 0;

#[derive(Copy, Clone, PartialEq)]
enum Move {
    Rock, Paper, Scissors
}

impl Move {
    fn to_win(&self) -> Move {
        match self {
            Move::Rock => Move::Paper,
            Move::Paper => Move::Scissors,
            Move::Scissors => Move::Rock
        }
    }

    fn to_lose(&self) -> Move {
        self.to_win().to_win()
    }

    fn to_tie(&self) -> Move {
        *self
    }

    fn weight(&self) -> u32 {
        match self {
            Move::Rock => 1,
            Move::Paper => 2,
            Move::Scissors => 3
        }
    }

    fn compare(&self, other: Move) -> u32 {
        let result = if self == &other {
            TIE
        } else {
            if self == &other.to_win() {
                WIN
            } else {
                LOSS
            }
        };
        result + self.weight()
    }
}


impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Move::Rock),
            "B" => Ok(Move::Paper),
            "C" => Ok(Move::Scissors),
            "X" => Ok(Move::Rock),
            "Y" => Ok(Move::Paper),
            "Z" => Ok(Move::Scissors),
            _   => Err(())
        }
    }
}


fn parse_str_to_function(text: &str) -> Result<fn(&Move) -> Move, &str> {
    match text {
        "X" => Ok(Move::to_lose),
        "Y" => Ok(Move::to_tie),
        "Z" => Ok(Move::to_win),
        _   => Err("Wrong key")
    }
}


pub fn day2() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken file");
    let lines: Vec<&str> = contents.split("\n").collect();
    let part1: u32 = lines.iter()
        .map(|x| {
            let row = x.split_once(" ");
            match row {
                Some((l, r)) => {
                    r.parse::<Move>()
                        .unwrap()
                        .compare(l.parse::<Move>().unwrap())
                },
                _ => 0
            }
        }).sum();
    let part2: u32 = lines.iter()
        .map(|x| {
            let row = x.split_once(" ");
            match row {
                Some((l, r)) => {
                    let their_move = l.parse::<Move>().unwrap();
                    parse_str_to_function(r).unwrap()(&their_move).compare(their_move)
                },
                _ => 0
            }
        }).sum();
    println!("Day 2:");
    println!("Part 1: {:?}", part1);
    println!("Part 2: {:?}", part2);
}
