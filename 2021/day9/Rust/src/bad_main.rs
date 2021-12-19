// use std::cmp::Ordering;
// use std::collections::{HashMap, HashSet};
// use std::fs;
//
// use itertools::Itertools;
// use petgraph::algo::connected_components;
// use petgraph::{Graph, Undirected};
// use petgraph::graph::NodeIndex;
// use petgraph::graph::UnGraph;
// use petgraph::unionfind::UnionFind;
// use petgraph::visit::{EdgeRef, NodeIndexable};
//
// fn main() {
// 	let raw = fs::read_to_string("inputT.txt").unwrap();
// 	let grid = raw.lines()
// 		.map(|line| {
// 			line.chars()
// 				.map(|c| c.to_string().parse::<i32>().unwrap())
// 				.collect::<Vec<_>>()
// 		})
// 		.collect::<Vec<_>>();
//
// 	let mut low_points = Vec::<i32>::new();
// 	for i in 0..grid.len() {
// 		for j in 0..grid[i].len() {
// 			let cell = grid[i][j];
// 			let up = i as isize - 1 < 0 || grid[i - 1][j] > cell;
// 			let down = i + 1 >= grid.len() || grid[i + 1][j] > cell;
// 			let left = j as isize - 1 < 0 || grid[i][j - 1] > cell;
// 			let right = j + 1 >= grid[i].len() || grid[i][j + 1] > cell;
//
// 			// println!("{} {} {} {} {}", cell, up, down, left, right);
// 			if up && down && left && right {
// 				low_points.push(cell);
// 			}
// 		}
// 	}
//
// 	let risk: i64 = low_points.iter().map(|height| 1 + *height as i64).sum();
//
// 	println!("{:?}", grid);
// 	println!("{:?}", low_points);
// 	println!("{}", risk);
//
//
// 	type Pos = (usize, usize);
// 	// let pos_lookup = grid.iter()
// 	//     .enumerate()
// 	//     .flat_map(|(y, row)| row.iter().enumerate().map(move |(x, cell)| (x, y)))
// 	//     .collect::<Vec<Pos>>();
//
// 	let mut edges = Vec::new();
// 	for i in 0..grid.len() {
// 		for j in 0..grid[i].len() {
// 			let node = (j, i);
// 			let cell = grid[i][j];
// 			if cell == 9 { continue; } // No basin
//
// 			let up = if i as isize - 1 <= 0 { None } else { Some((j, i - 1)) };
// 			let down = if i + 1 >= grid.len() { None } else { Some((j, i + 1)) };
// 			let left = if j as isize - 1 <= 0 { None } else { Some((j - 1, i)) };
// 			let right = if j + 1 >= grid[i].len() { None } else { Some((j + 1, i)) };
//
// 			for pos in [up, down, left, right] {
// 				if let Some((x, y)) = pos {
// 					if grid[y][x] == 9 { continue; }
// 					edges.push((NodeIndex::new(index_of(&pos_lookup, node)), NodeIndex::new(index_of(&pos_lookup, (x, y)))));
// 				}
// 			}
// 		}
// 	}
// 	// let graph = UnGraph::<usize, ()>::from_edges(edges.as_slice());
// 	// let c = connected(&graph).into_iter().map(|idx| pos_lookup[idx]).collect::<Vec<_>>();
//
// 	println!("{:?}", c);
//
// 	fn index_of<T: Copy + PartialEq>(v: &Vec<T>, e: T) -> usize {
// 		v.iter().position(|a| *a == e).unwrap()
// 	}
// 	// let mut adj_l: HashMap<Pos, Vec<Pos>> = HashMap::new();
// 	// for i in 0..grid.len() {
// 	//     for j in 0..grid[i].len() {
// 	//         let cell = grid[i][j];
// 	//         let mut connected = Vec::<Pos>::new();
// 	//         if cell == 9 { continue; }
// 	//
// 	//         let up = if i as isize - 1 <= 0 { (9, None) } else { (grid[i - 1][j], Some((j, i - 1))) };
// 	//         let down = if i + 1 >= grid.len() { (9, None) } else { (grid[i + 1][j], Some((j, i + 1))) };
// 	//         let left = if j as isize - 1 <= 0 { (9, None) } else { (grid[i][j - 1], Some((j - 1, i))) };
// 	//         let right = if j + 1 >= grid[i].len() { (9, None) } else { (grid[i][j + 1], Some((j + 1, i))) };
// 	//
// 	//         // println!("{} {:?} {:?} {:?} {:?}", cell, up, down, left, right);
// 	//         for (n, pos) in [up, down, left, right] {
// 	//             if n == 9 { continue; }
// 	//             if let Some(pos) = pos {
// 	//                 connected.push(pos);
// 	//             }
// 	//         }
// 	//         adj_l.insert((j, i), connected);
// 	//     }
// 	// }
//
// 	// let mut seen: HashSet<Pos> = HashSet::new();
// 	// adj_l.iter()
// 	//     .sorted_by(|(node1, neigh1), (node2, neigh2)| {
// 	//         if neigh1.len() < neigh2.len() {
// 	//             Ordering::Greater
// 	//         } else {
// 	//             Ordering::Less
// 	//         }
// 	//     })
// 	//     .filter(|(node, neigh)| {
// 	//         if seen.contains(node) || neigh.iter().any(|p| seen.contains(p)) {
// 	//            return false;
// 	//         }
// 	//         seen.insert(**node);
// 	//         neigh.iter().for_each(|p| { seen.insert(*p); });
// 	//         true
// 	//     })
// 	//     .take(3)
// 	//     .map(|(node, neigh)| {
// 	//         println!("aa {:?}", neigh);
// 	//         neigh.len() + 1
// 	//     })
// 	//     .for_each(|a| println!("idk {:?}", a));
// }
