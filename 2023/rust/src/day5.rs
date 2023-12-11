use itertools::Itertools;

static FILE_PATH: &'static str = "../data/day5.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH)
        .expect("Broken file");
    println!("Day 5");
    part1(&contents);
    part2(&contents);
}

fn text_to_seeds(text: &str) -> Vec<u64> {
    text.lines()
        .find(|l| l.starts_with("seeds"))
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(' ')
        .map(|t| t.parse().unwrap())
        .collect()
}

fn text_to_seed_ranges(text: &str) -> Vec<SeedRange> {
    text.lines()
        .find(|l| l.starts_with("seeds"))
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(' ')
        .map(|t| t.parse().unwrap())
        .tuples::<(u64, u64)>()
        .map(|(a, b)| SeedRange { number: a, length: b})
        .collect()
}

fn text_to_map(text: &str) -> Vec<Vec<(u64, u64, u64)>> {
    let mut maps = Vec::new();
    let mut sub_map = Vec::new();

    // hacky work around
    let new_text = format!("{text}\n:");

    for line in new_text
    .lines()
    .filter(|l| !l.is_empty()) {
        if line.contains(':') {
            if !sub_map.is_empty() {
                maps.push(sub_map.drain(..).collect());
            }
        } else {
            let next = line.split(' ')
                .map(|t| t.parse::<u64>().unwrap())
                .tuples::<(u64, u64, u64)>()
                .next()
                .unwrap();

            sub_map.push(next)
        }
    }

    maps
}

fn text_to_map_structured(text: &str) -> Vec<Vec<Translation>> {
    let mut maps = Vec::new();
    let mut sub_map = Vec::new();

    // hacky work around
    let new_text = format!("{text}\n:");

    for line in new_text
    .lines()
    .filter(|l| !l.is_empty()) {
        if line.contains(':') {
            if !sub_map.is_empty() {
                maps.push(sub_map.drain(..).collect());
            }
        } else {
            let next = line.splitn(3, ' ')
                .map(|t| t.parse::<u64>().unwrap())
                .tuples::<(u64, u64, u64)>()
                .next().unwrap();
            sub_map.push(
                Translation { dest: next.0 , start: next.1, range: next.2 }
            )
        }
    }

    maps
}

#[derive(Clone, Copy, Debug)]
struct SeedRange {
    number: u64,
    length: u64
}

impl SeedRange {
    fn range(&self) -> (u64, u64) {
        (self.number, self.number + self.length)
    }

    fn try_build_from_range(a: u64, b: u64) -> Option<SeedRange> {
        if b <= a {
            None
        } else {
            Some(SeedRange { number: a, length: b - a })
        }
    }

    // Returns (translated_intervals, untranslated_intervals)
    // Seed interval: (x, y)
    // Translation interval: (a, b)
    // If the intervals overlap, then the translated interval is
    //      (max(a, x), min(b, y)
    //  and the sub intervals that were not translated are
    //      (x, a) -- the before chunk
    //      (b, y) -- the after chunk
    //  empty intervals are excluded of course
    fn through_translation(
        &self, translation: &Translation
    ) -> (Vec<SeedRange>, Vec<SeedRange>) {
        let (seed_a, seed_b) = self.range();
        let (translation_a, translation_b) = translation.start_range();

        if let Some(mut translated) = SeedRange::try_build_from_range(
            seed_a.max(translation_a),
            seed_b.min(translation_b)
        ) {
            translated.number = seed_a.checked_sub(translation_a).unwrap_or(0)
                + translation.dest;
            let untranslated = [
                SeedRange::try_build_from_range(
                    seed_a, translation_a.checked_sub(1).unwrap_or(0)
                ),
                SeedRange::try_build_from_range(
                    translation_b + 1, seed_b
                )
            ]
            .into_iter()
            .filter_map(|range| range)
            .collect();

            (vec![translated], untranslated)
        } else {
            (vec![], vec![self.to_owned()])
        }

    }

    // Recursively iterate through translations, breaking off the untranslated intervals
    // and sending those through the remaining translations
    fn through_map(&self, map: &[Translation]) -> Vec<SeedRange> {
        if let Some((translation, rem)) = map.split_first() {
            let (mut translated, untranslated) = self.through_translation(translation);
            let rem_translated: Vec<SeedRange> = untranslated
                .into_iter()
                .map(|range| range.through_map(rem))
                .flatten()
                .collect();
            translated.extend(rem_translated);
            translated
        } else { vec![*self] }
    }

    fn through_maps(&self, maps: &[Vec<Translation>]) -> Vec<SeedRange> {
        let init = vec![*self];
        maps.iter()
            .fold( init, |acc, map| {
                let result = acc
                    .iter()
                    .map(|range| range.through_map(map))
                    .flatten()
                    .collect_vec();
                result
            })
    }
}

#[derive(Debug)]
struct Translation {
    dest: u64,
    start: u64,
    range: u64
}

impl Translation {
    fn start_range(&self) -> (u64, u64) {
        (self.start, self.start + self.range)
    }
}


fn evaluate(seeds: Vec<u64>, map: Vec<Vec<(u64, u64, u64)>>) -> u64 {
    seeds
        .iter()
        .map( |&seed|
            map.iter()
            .fold(seed, |acc, new| {
                new.iter()
                    .find_map(|(dest, start, range)| {
                            if acc.ge(start) && (acc - start).le(range) {
                                Some(dest + (acc - start))
                            } else { None }
                        }
                    ).unwrap_or(acc)
            })
        )
        .min()
        .unwrap()
}


fn evaluate_ranges(seed_ranges: &[SeedRange], maps: &[Vec<Translation>]) -> u64 {
    seed_ranges.iter()
        .map( |range| range
            .through_maps(&maps)
            .iter()
            .map(|sub_range| sub_range.number)
            .min()
            .unwrap()
        ).min().unwrap()
}


fn part1(contents: &str) -> u64 {
    let seeds = text_to_seeds(contents);
    let map = text_to_map(contents);
    let answer = evaluate(seeds, map);
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u64 {
    let seed_ranges = text_to_seed_ranges(contents);
    let map = text_to_map_structured(contents);
    let answer = evaluate_ranges(&seed_ranges, &map);
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::*;

    static DATA: &'static str = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";
    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 35);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 46);
    }
}

