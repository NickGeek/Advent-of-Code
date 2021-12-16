use std::collections::HashMap;
use petgraph::Graph;
use petgraph::graph::UnGraph;

fn main() {
    part1();
    part2();
}

fn part1() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let mut graph = Graph::<(), i64>::new();
    let mut node_map = HashMap::new();

    let mut max_x = 0;
    let mut max_y = 0;
    raw.lines()
        .enumerate()
        .flat_map(|(y, row)| row.chars().enumerate().map(move |(x, cost)| (x, y, cost.to_string().parse::<i64>().unwrap())))
        .for_each(|(x, y, cost)| {
            let node = graph.add_node(());
            node_map.insert((x, y), node);
            if x > max_x { max_x = x }
            if y > max_y { max_y = y }
        });

    let mut grid = vec![vec![-1; max_x + 1]; max_y + 1];
    for (y, row) in raw.lines().enumerate() {
        for (x, cost) in row.chars().enumerate() {
            grid[y][x] = cost.to_string().parse::<i64>().unwrap();
        }
    }

    for pos in node_map.keys() {
        let (x, y) = *pos;
        for n_x in (x as isize-1)..=(x as isize+1) {
            if n_x < 0 { continue; }
            let n_x = n_x as usize;
            if let Some(b) = node_map.get(&(n_x, y)) {
                let b = b.clone();
                graph.add_edge(node_map.get(&(x, y)).unwrap().clone(), b, grid[y][n_x]);
            }
        }
        for n_y in (y as isize-1)..=(y as isize+1) {
            if n_y < 0 { continue; }
            let n_y = n_y as usize;
            if let Some(b) = node_map.get(&(x, n_y)) {
                let b = b.clone();
                graph.add_edge(node_map.get(&(x, y)).unwrap().clone(), b, grid[n_y][x]);
            }
        }
    }

    let start = node_map.get(&(0, 0)).unwrap();
    let goal = node_map.get(&(max_x, max_y)).unwrap();

    let res = petgraph::algo::dijkstra(&graph, start.clone(), Some(goal.clone()), |e| {
        *e.weight()
    });

    let res = *res.get(goal).unwrap();

    println!("{:?}", res);
}

fn part2() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let mut graph = Graph::<(), i64>::new();
    let mut node_map = HashMap::new();

    let mut max_x = 0;
    let mut max_y = 0;
    raw.lines()
        .enumerate()
        .flat_map(|(y, row)| row.chars().enumerate().map(move |(x, cost)| (x, y, cost.to_string().parse::<i64>().unwrap())))
        .for_each(|(x, y, cost)| {
            let node = graph.add_node(());
            node_map.insert((x, y), node);
            if x > max_x { max_x = x }
            if y > max_y { max_y = y }
        });

    let max_modi = 5;
    let mut grid = vec![vec![-1; (max_x * max_modi) + max_modi]; (max_y * max_modi) + max_modi];
    // Fill in initial section
    let mut initial = vec![vec![-1; (max_x + 1)]; (max_y + 1)];
    for (y, row) in raw.lines().enumerate() {
        for (x, cost) in row.chars().enumerate() {
            initial[y][x] = cost.to_string().parse::<i64>().unwrap();
            grid[y][x] = cost.to_string().parse::<i64>().unwrap();
        }
    }

    // let new_points = (0..(max_modi * grid.len()))
    //     .map(|y| {
    //         (0..(max_modi * grid[0].len())).map(|x| {
    //             let x_modi = (x / initial.len()) as i64;
    //             let y_modi = (x / initial[0].len()) as i64;
    //             let cost = initial[y % initial.len()][x % initial[0].len()] + x_modi + y_modi;
    //             let cost = if cost < 10 { cost } else { cost - 9 };
    //
    //             let node = graph.add_node(());
    //             node_map.insert((x, y), node);
    //
    //             cost
    //         }).collect::<Vec<_>>()
    //     })
    //     .collect::<Vec<_>>();

    // println!("{:?}", new_points);

    let grid = (0..(max_modi*initial.len()))
        .map(|y| {
            (0..(max_modi * initial[0].len()))
                .map(|x| {
                    let cost_modi = (y / initial[0].len()) as i64 + (x / initial.len()) as i64;
                    let cost = initial[y % initial.len()][x % initial[0].len()] + cost_modi;
                    if cost < 10 { cost } else { cost - 9 }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    // for i in 0..(max_modi - 1) {
    //     let modi = i + 1;
    //     for y in 0..initial.len() {
    //         for x in 0..initial[y].len() {
    //             let mut cost = initial[y][x];
    //             for _ in 0..modi {
    //                 cost += 1;
    //                 if cost > 9 { cost = 1; }
    //             }
    //
    //             grid[y + (max_y + 1) * modi][x] = cost;
    //             let node = graph.add_node(());
    //             println!("ins: {:?}", (x, y + (max_y + 1) * modi));
    //             node_map.insert((x, y + (max_y + 1) * modi), node);
    //
    //             grid[y][x + (max_x + 1) * modi] = cost;
    //             let node = graph.add_node(());
    //             println!("ins: {:?}", (x + (max_x + 1) * modi, y));
    //             node_map.insert((x + (max_x + 1) * modi, y), node);
    //         }
    //         break;
    //     }
    // }

    // println!("{:#?}", grid);

    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let n = graph.add_node(());
            node_map.insert((x, y), n);
        }
    }

    for pos in node_map.keys() {
        let (x, y) = *pos;
        for n_x in (x as isize-1)..=(x as isize+1) {
            if n_x < 0 { continue; }
            let n_x = n_x as usize;
            if let Some(b) = node_map.get(&(n_x, y)) {
                let b = b.clone();
                graph.add_edge(node_map.get(&(x, y)).unwrap().clone(), b, grid[y][n_x]);
            }
        }
        for n_y in (y as isize-1)..=(y as isize+1) {
            if n_y < 0 { continue; }
            let n_y = n_y as usize;
            if let Some(b) = node_map.get(&(x, n_y)) {
                let b = b.clone();
                graph.add_edge(node_map.get(&(x, y)).unwrap().clone(), b, grid[n_y][x]);
            }
        }
    }

    // println!("{:?}", grid[0].iter().map(|a| a.to_string()).collect::<String>());

    let start = node_map.get(&(0, 0)).unwrap();
    let goal = node_map.get(&((max_x + 1) * max_modi - 1, (max_y + 1) * max_modi - 1)).unwrap();


    let res = petgraph::algo::dijkstra(&graph, start.clone(), Some(goal.clone()), |e| {
        *e.weight()
    });

    let res = *res.get(goal).unwrap();

    println!("{:?}", res);
}
