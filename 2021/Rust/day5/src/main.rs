use std::cmp::{max, min};
use std::fs;
use anyhow::Result;

type Grid = Vec<Vec<usize>>;
type CoOrds = (usize, usize, usize, usize);

fn main() -> Result<()> {
    let raw = fs::read_to_string("input.txt")?;

    let input: Vec<CoOrds> = raw.lines()
        .map(|line| {
            let data: Vec<usize> = line.replace(" -> ", ",").split(',').take(4).map(|s_int| s_int.parse::<usize>().unwrap()).collect();
            (data[0], data[1], data[2], data[3])
        })
        .collect();

    let mut max_x = 0;
    let mut max_y = 0;
    for (f_x, f_y, t_x, t_y) in input.iter() {
        max_x = max(max_x, max(*f_x, *t_x));
        max_y = max(max_y, max(*f_y, *t_y));
    }

    let grid = vec![vec![0; max_x + 1]; max_y + 1];

    println!("Danger Zones Part 1: {}", part_1(grid.clone(), &input));
    println!("Danger Zones Part 2: {}", part_2(grid, input));
    Ok(())
}

#[allow(dead_code)]
fn part_1(mut grid: Grid, input: &[CoOrds]) -> usize {
    let part1_lines: Vec<CoOrds> = input.iter().filter(|(f_x, f_y, t_x, t_y)| *f_x == *t_x || *f_y == *t_y).copied().collect();
    for (f_x, f_y, t_x, t_y) in part1_lines {
        for row_i in min(f_y, t_y)..=max(f_y, t_y) {
            for col_i in min(f_x, t_x)..=max(f_x, t_x) {
                let cell = grid.get_mut(row_i)
                    .and_then(|row: &mut Vec<usize>| row.get_mut(col_i))
                    .unwrap();
                *cell += 1;
            }
        }
    }

    let danger_zones = grid.iter()
        .flat_map(|row| row.iter())
        .filter(|cell| **cell >= 2)
        .count();
    danger_zones
}

#[allow(dead_code)]
fn part_2(mut grid: Grid, input: Vec<CoOrds>) -> usize {
    for (f_x, f_y, t_x, t_y) in input {
        let mut c_x = f_x; let mut c_y = f_y;
        loop {
            let cell = grid.get_mut(c_y)
                .and_then(|row: &mut Vec<usize>| row.get_mut(c_x))
                .unwrap();
            *cell += 1;

            if c_x == t_x && c_y == t_y {
                break;
            }

            if c_x != t_x {
                if c_x < t_x {
                    c_x += 1;
                } else {
                    c_x -= 1;
                }
            }
            if c_y != t_y {
                if c_y < t_y {
                    c_y += 1;
                } else {
                    c_y -= 1;
                }
            }
        }
    }

    // println!("{:?}", grid);

    let danger_zones = grid.iter()
        .flat_map(|row| row.iter())
        .filter(|cell| **cell >= 2)
        .count();

    danger_zones
}
