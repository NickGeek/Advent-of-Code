use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fs;

use itertools::Itertools;

type Pos = (usize, usize);

fn main() {
    let raw = fs::read_to_string("input.txt").unwrap();
    let grid = raw.lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut low_points = Vec::<i32>::new();
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            let cell = grid[i][j];
            let up = i as isize - 1 < 0 || grid[i - 1][j] > cell;
            let down = i + 1 >= grid.len() || grid[i + 1][j] > cell;
            let left = j as isize - 1 < 0 || grid[i][j - 1] > cell;
            let right = j + 1 >= grid[i].len() || grid[i][j + 1] > cell;

            // println!("{} {} {} {} {}", cell, up, down, left, right);
            if up && down && left && right {
                low_points.push(cell);
            }
        }
    }

    let risk: i64 = low_points.iter().map(|height| 1 + *height as i64).sum();

    println!("{:?}", grid);
    println!("{:?}", low_points);
    println!("{}", risk);


    let mut positions = grid.iter()
        .enumerate()
        .flat_map(|(y, row)| row.iter().enumerate().map(move |(x, cell)| (x, y)))
        .collect::<Vec<Pos>>();

    // DFS to find
    let mut visited = HashSet::with_capacity(positions.len());
    // let mut basin_sizes = Vec::new();
    let res: usize = positions.into_iter()
        .map(|pos| {
            let procd = visited.len();
            dfs(&grid, &mut visited, pos);
            return visited.len() - procd;
        })
        .sorted()
        .rev()
        .take(3)
        .product();

    println!("{:?}", res);
}

fn dfs(graph: &Vec<Vec<i32>>, visited: &mut HashSet<Pos>, pos: Pos) -> bool {
    if visited.contains(&pos) || graph[pos.1][pos.0] == 9 {
        return false;
    }
    visited.insert(pos);

    let (j, i) = pos;
    if i as isize - 1 >= 0 { dfs(graph, visited, (j, i - 1)); };
    if i + 1 < graph.len() { dfs(graph, visited, (j, i + 1)); };
    if j as isize - 1 >= 0 { dfs(graph, visited, (j - 1, i)); };
    if j + 1 < graph[i].len() { dfs(graph, visited, (j + 1, i)); };
    true
}
