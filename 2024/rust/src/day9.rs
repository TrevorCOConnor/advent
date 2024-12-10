use std::collections::HashSet;

static FILE_PATH: &'static str = "../data/day9.txt";

pub fn solutions() {
    let contents = std::fs::read_to_string(FILE_PATH).expect("Broken File");
    println!("Day 9");
    part1(&contents);
    part2(&contents);
}

#[derive(Clone, Copy, Debug)]
struct FileInfo {
    file_id: Option<usize>,
    length: u32,
}

impl FileInfo {
    fn intersection(&self, rhs: &Self) -> Self {
        FileInfo {
            file_id: rhs.file_id,
            length: rhs.length.min(self.length),
        }
    }

    fn difference(&self, rhs: &Self) -> Self {
        FileInfo {
            file_id: self.file_id,
            length: (self.length.checked_sub(rhs.length).unwrap_or(0)),
        }
    }
}

fn read_data(contents: &str) -> Vec<FileInfo> {
    contents
        .chars()
        .filter(|c| !c.is_whitespace())
        .enumerate()
        .map(|(e, v)| {
            let file_id = {
                if e % 2 == 0 {
                    Some(e / 2)
                } else {
                    None
                }
            };
            let length = v.to_digit(10).unwrap();
            FileInfo { file_id, length }
        })
        .collect()
}

fn reorder_file_chunks(file_info: &[FileInfo]) -> Vec<FileInfo> {
    let mut files = file_info.iter().cloned().enumerate();
    let mut rev_files = file_info.iter().cloned().enumerate().rev();

    let mut reorder: Vec<FileInfo> = Vec::new();
    let mut left = files.next().unwrap();
    let mut right = rev_files.next().unwrap();

    while left.0 <= right.0 {
        let (l, left_file) = left;
        let (r, right_file) = right;

        // the tips have touched
        // if a right grouping was still being processed add the remnants
        if l == r {
            if right_file.file_id.is_some() {
                reorder.push(right_file);
                break;
            }
        }

        // Don't move non empty space
        if left_file.file_id.is_some() {
            reorder.push(left_file);
            left = files.next().unwrap();
            continue;
        }

        // Remove 0 length blocks
        if left_file.length == 0 {
            left = files.next().unwrap();
            continue;
        }

        // Skip empty space and zero length spaces on the right
        if right_file.file_id.is_none() || right_file.length == 0 {
            right = rev_files.next().unwrap();
            continue;
        }

        // Combine the FileInfos
        let addition = left_file.intersection(&right_file);
        reorder.push(addition);

        left = (l, left_file.difference(&right_file));
        right = (r, right_file.difference(&left_file));
    }

    reorder
}

fn reorder_whole_files(file_info: &[FileInfo]) -> Vec<FileInfo> {
    let mut files: Vec<FileInfo> = file_info.iter().cloned().collect();
    let mut idx = 0;

    loop {
        let rev_idx = files.len() - (idx + 1);
        if rev_idx == 0 {
            break;
        }

        let item = files[rev_idx];
        if item.file_id.is_some() {
            if let Some((empty_idx, &empty_space)) = files[..=rev_idx]
                .iter()
                .enumerate()
                .find(|(_, f)| f.file_id.is_none() && f.length >= item.length)
            {
                files.get_mut(rev_idx).unwrap().file_id.take();
                files.remove(empty_idx);
                let rem = empty_space.difference(&item);
                if rem.length > 0 {
                    files.insert(empty_idx, rem);
                }
                files.insert(empty_idx, item);
            }
        }
        idx += 1;
    }
    files
}

fn calculate_check_sum(file_info: &[FileInfo]) -> u64 {
    let mut counter: u32 = 0;
    let mut check_sum: u64 = 0;
    for info in file_info.iter() {
        if info.file_id.is_some() {
            // speed up later... maybe
            let addition =
                (counter..counter + info.length).sum::<u32>() * (info.file_id.unwrap() as u32);
            check_sum += addition as u64;
        }
        counter += info.length;
    }
    check_sum
}

fn part1(contents: &str) -> u64 {
    let info = read_data(contents);
    let answer = calculate_check_sum(&reorder_file_chunks(&info));
    println!("Part 1: {}", answer);
    answer
}

fn part2(contents: &str) -> u64 {
    let info = read_data(contents);
    let answer = calculate_check_sum(&reorder_whole_files(&info));
    println!("Part 2: {}", answer);
    answer
}

#[cfg(test)]
mod test {
    use super::{part1, part2};

    static DATA: &'static str = "2333133121414131402";

    #[test]
    fn test_part1() {
        let answer = part1(DATA);
        assert_eq!(answer, 1928);
    }

    #[test]
    fn test_part2() {
        let answer = part2(DATA);
        assert_eq!(answer, 2858);
    }
}

