use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::format;
use petgraph::Graph;
use crate::Space::*;
use itertools::Itertools;
use petgraph::prelude::NodeIndex;

type Pos = (i64, i64);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Space {
    A,
    B,
    C,
    D,
    Wall,
    Empty,
    Hallway,
    Ignore
}

// Plan: use dijkstra with changing edge costs based on the state of the world

fn main() {
    let raw = std::fs::read_to_string("inputT.txt").unwrap();

    let nodes: HashMap<Pos, Space> = raw.lines().enumerate()
        .flat_map(|(y, l)| {
            l.chars().enumerate().map(move |(x, c)| {
                ((x as i64, y as i64), match c {
                    '#' => Wall,
                    '.' if y > 1 => Empty,
                    '.' if y == 1 => Hallway,
                    'A' => A,
                    'B' => B,
                    'C' => C,
                    'D' => D,
                    _ => Ignore
                })
            })
        })
        .filter(|(pos, space)| {
            match space {
                Ignore => false,
                _ => true
            }
        })
        .collect();

    let adj_list = gen_adj_list(&nodes);

    // println!("{:?}", adj_list);


    let mut graph = Graph::<(), i64>::new();
    let start = graph.add_node(());
    let mut visited = HashSet::new();
    build_graph(&mut graph, &nodes, start, &mut visited);

    // println!("{:?}", graph);

    // let mut moves = generate_moves(&nodes, &adj_list);
    // for (cost, next) in moves {
    //     let next_node = graph.add_node(());
    //     graph.add_edge(prev, next_node, cost as i64);
    // }
}

fn build_graph(graph: &mut Graph<(), i64>, prev: &HashMap<Pos, Space>, from: NodeIndex, visited: &mut HashSet<String>) {
    if is_complete(prev) {
        println!("{}", hash_the_map(prev));
    }

    if visited.contains(&*hash_the_map(prev)) {
        return;
    }

    // println!("idk\n{}\n", hash_the_map(prev));

    visited.insert(hash_the_map(prev));

    let from = graph.add_node(());

    let moves = generate_moves(prev, &gen_adj_list(prev));
    for (cost, next) in moves {
        let next_node = graph.add_node(());
        graph.add_edge(from, next_node.clone(), cost as i64);

        build_graph(graph, &next, next_node, visited);
    }
}

fn gen_adj_list(nodes: &HashMap<Pos, Space>) -> HashMap<Pos, Vec<Pos>> {
    let mut adj_list = HashMap::new();

    for (pos, _) in nodes.iter() {
        let (x, y) = *pos;
        let mut neighbours = vec![];

        for neighbour in [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] {
            if let Some(node) = nodes.get(&neighbour) {
                neighbours.push(neighbour);
            }
        }
        adj_list.insert(*pos, neighbours);
    }

    adj_list
}

fn generate_moves(nodes: &HashMap<Pos, Space>, adj_list: &HashMap<Pos, Vec<Pos>>) -> Vec<(u64, HashMap<Pos, Space>)> {
    let mut moves = vec![];
    if is_complete(nodes) {
        return moves;
    }

    let mut visited = HashSet::new();

    for (pos, space) in nodes {
        match space {
            Wall | Ignore | Hallway | Empty => continue,
            _ => {
                let mut new_moves = calc_movement(nodes, adj_list, (*pos, *space), &mut visited);
                moves.append(&mut new_moves);
            }
        };
    }

    moves
}

fn calc_movement(nodes: &HashMap<Pos, Space>, adj_list: &HashMap<Pos, Vec<Pos>>, (pos, space): (Pos, Space), visited: &mut HashSet<String>) -> Vec<(u64, HashMap<Pos, Space>)> {
    let neighbours = adj_list.get(&pos).unwrap();
    let mut moves = vec![];

    let is_in_hallway = pos.1 == 1;
    let is_in_room = pos.1 >= 2;

    for neigh_pos in neighbours {
        let neigh = nodes.get(neigh_pos).unwrap();
        match neigh {
            Wall | Ignore => continue,
            Hallway => {
                if is_my_empty_room(&space, pos, nodes) { continue; }

                // Can't stop outside rooms
                if neigh_pos.0 == 3 || neigh_pos.0 == 5 || neigh_pos.0 == 7 || neigh_pos.0 == 9 {
                    let mut movement = nodes.clone();
                    movement.insert(pos, Hallway);
                    movement.insert(*neigh_pos, space);
                    let cost = get_cost(&space);

                    if visited.contains(&hash_the_map(&movement)) {
                        continue;
                    }
                    visited.insert(hash_the_map(&movement));

                    let next = calc_movement(&movement, &gen_adj_list(&movement), (*neigh_pos, space), visited);
                    if next.is_empty() {
                        continue;
                    } else {
                        moves.append(&mut next.into_iter()
                            .map(|(plan_cost, plan)| (plan_cost + cost, plan))
                            .collect());
                    }
                } else {
                    let mut movement = nodes.clone();
                    movement.insert(pos, Hallway);
                    movement.insert(*neigh_pos, space);
                    let cost = get_cost(&space);

                    if visited.contains(&hash_the_map(&movement)) {
                        continue;
                    }
                    visited.insert(hash_the_map(&movement));

                    let next = calc_movement(&movement, &gen_adj_list(&movement), (*neigh_pos, space), visited);
                    moves.push((cost, movement));
                    if !next.is_empty() {
                        moves.append(&mut next.into_iter()
                            .map(|(plan_cost, plan)| (plan_cost + cost, plan))
                            .collect());
                    }
                }
            },
            Empty => {
                if neigh_pos.1 < pos.1 && is_my_empty_room(&space, *neigh_pos, nodes) {
                    let mut movement = nodes.clone();
                    movement.insert(pos, Empty);
                    movement.insert(*neigh_pos, space);
                    let cost = get_cost(&space);
                    moves.push((cost, movement));
                }
            }
            _ => continue
        }
    }

    moves
}

fn hash_the_map(map: &HashMap<Pos, Space>) -> String {
    let mut res = String::new();

    for y in 0..4 {
        for x in 0..13 {
            let default = Ignore;
            let node = map.get(&(x, y)).unwrap_or(&Ignore);
            res.push(match node {
                A => 'A',
                B => 'B',
                C => 'C',
                D => 'D',
                Wall => '#',
                Empty | Hallway => '.',
                Ignore => ' '
            })
        }
        res.push('\n');
    }

    res
}

fn get_cost(space: &Space) -> u64 {
    match space {
        A => 1,
        B => 10,
        C => 100,
        D => 1000,
        _ => 0
    }
}

fn is_my_empty_room(amphi: &Space, (x, y): Pos, nodes: &HashMap<Pos, Space>) -> bool {
    let node = nodes.get(&(x, y)).unwrap();
    if let Empty = node {
        match amphi {
            A => {
                x == 3 && (y == 2 || y == 3)
            }
            B => {
                x == 5 && (y == 2 || y == 3)
            }
            C => {
                x == 7 && (y == 2 || y == 3)
            }
            D => {
                x == 9 && (y == 2 || y == 3)
            }
            _ => false
        }
    } else {
        false
    }
}

fn is_complete(nodes: &HashMap<Pos, Space>) -> bool {
    let a = nodes.get(&(3,2)).unwrap() == &A && nodes.get(&(3,3)).unwrap() == &A;
    let b = nodes.get(&(5,2)).unwrap() == &A && nodes.get(&(5,3)).unwrap() == &B;
    let c = nodes.get(&(7,2)).unwrap() == &A && nodes.get(&(7,3)).unwrap() == &C;
    let d = nodes.get(&(9,2)).unwrap() == &A && nodes.get(&(9,3)).unwrap() == &D;
    a && b && c && d
}
