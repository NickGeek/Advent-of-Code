use std::cmp::{max, min};
use std::collections::HashSet;
use std::hash::Hash;

type Pos = (i32, i32);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Axis {
    X,
    Y
}

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let (dots, folds) = raw.split_once("\n\n").unwrap();

    let mut dots = dots.lines()
        .map(|pos| -> Pos {
            let (x, y) = pos.split_once(',').unwrap();
            (x.parse::<i32>().unwrap(), y.parse::<i32>().unwrap())
        })
        .collect::<HashSet<_>>();

    // let mut max_x = 0;
    // let mut max_y = 0;
    // for (x, y) in dots.iter() {
    //     if *x > max_x { max_x = *x; }
    //     if *y > max_y { max_y = *y; }
    // }
    //
    // let mut grid = vec![vec![false; max_x + 1]; max_y + 1];
    // for (x, y) in dots.iter() { grid[*y][*x] = true; }

    // let mut next = grid;
    println!("{:?}", dots);
    for fold in folds.lines() {
        let fold = fold.split(' ').last().unwrap();
        let (axis, n) = fold.split_once('=').unwrap();
        let n = n.parse::<i32>().unwrap();

        // next = fold_step(next.as_slice(), axis, n);

        dots = fold_step_v2(&dots, axis, n);
        println!("{:?}", dots);
    }

    let hashes = dots.len();

    println!("{:?}", hashes);

    let mut max_x: usize = 0;
    let mut max_y: usize = 0;
    for (x, y) in dots.iter() {
        if *x > max_x as i32 { max_x = *x as usize; }
        if *y > max_y as i32 { max_y = *y as usize; }
    }

    let mut grid = vec![vec!['.'; max_x + 1]; max_y + 1];
    for (x, y) in dots.iter() { grid[*y as usize][*x as usize] = '#'; }
    for row in grid {
        println!("{}", row.into_iter().collect::<String>());
    }
}

fn fold_step_v2(points: &HashSet<Pos>, axis: &str, fold_n: i32) -> HashSet<Pos> {
    if axis == "y" {
        points.iter()
            .map(|(og_x, og_y)| {
                let x = *og_x;
                let y = fold_n - (*og_y - fold_n).abs();
                (x, y)
            })
            .collect::<HashSet<_>>()
    } else {
        points.iter()
            .map(|(og_x, og_y)| {
                let x = fold_n - (*og_x - fold_n).abs();
                let y = *og_y;
                (x, y)
            })
            .collect::<HashSet<_>>()
    }
}

// fn fold_step(grid: &[Vec<bool>], axis: &str, fold_n: i64) -> Vec<Vec<bool>> {
//     let new_y = if axis == "y" { fold_n } else { grid.len() };
//     let new_x = if axis == "x" { fold_n } else { grid[0].len() };
//     let mut new_grid = vec![vec![false; new_x]; new_y];
//     for y in 0..new_y {
//         for x in 0..new_x {
//             new_grid[y][x] = grid[y][x];
//         }
//     }
//
//     if axis == "y" {
//         let mut other_half = vec![vec![false; grid[0].len()]; grid.len() - (new_y + 1)];
//         for y in (new_y + 1)..grid.len() {
//             for x in 0..grid[y].len() {
//                 let new = grid[y][x];
//                 let f_y = y - (new_y + 1);
//                 other_half[f_y][x] = new;
//             }
//         }
//
//         // let other_half = other_half.into_iter().skip(1).collect::<Vec<_>>();
//         for y in (0..other_half.len()).rev() {
//             for x in 0..other_half[y].len() {
//                 let new = other_half[y][x];
//                 if new {
//                     new_grid[y][x] = new;
//                 }
//             }
//         }
//     } else {
//         println!("sdrsdrgedsrgedrg: {}", grid[0].len());
//         let mut other_half = vec![vec![false; (grid[0].len() - 1) - new_x]; grid.len()];
//         for y in 0..(grid.len() - 1) {
//             for x in new_x..(grid[y].len() - 1) {
//                 let new = grid[y][x];
//                 let f_x = x - new_x;
//                 other_half[y][f_x] = new;
//             }
//         }
//
//         println!("sfef");
//
//         // let other_half = other_half.into_iter().skip(1).collect::<Vec<_>>();
//         for y in 0..other_half.len() {
//             for x in (0..other_half[y].len()).rev() {
//                 let new = other_half[y][x];
//                 if new {
//                     new_grid[y][x] = new;
//                 }
//             }
//         }
//     }
//
//     println!("{}", new_grid.len());
//     new_grid
// }
