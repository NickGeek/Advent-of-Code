use std::collections::{HashMap, HashSet, VecDeque};
use std::f32::consts::E;
use crate::Cave::*;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum Cave {
    Big(String),
    Small(String),
    Start,
    End
}
impl From<&str> for Cave {
    fn from(s: &str) -> Self {
        match s {
            "start" => Start,
            "end" => End,
            _ => {
                if s.to_uppercase() == s {
                    Big(s.to_string())
                } else {
                    Small(s.to_string())
                }
            }
        }
    }
}

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();

    let mut adj_list = HashMap::<Cave, Vec<Cave>>::new();
    for line in raw.lines() {
        let (n1, n2) = line.split_once('-').unwrap();
        let n1 = Cave::from(n1);
        let n2 = Cave::from(n2);

        add_cave(&mut adj_list, n1.clone(), n2.clone());
        add_cave(&mut adj_list, n2, n1);
    }

    println!("{:?}", find_paths(&adj_list))
}

fn find_paths(graph: &HashMap<Cave, Vec<Cave>>) -> u64 {
    let mut n_paths = 0;

    let mut q = VecDeque::new();
    q.push_front((Start, HashSet::new(), false));

    while !q.is_empty() {
        let (node, visited_small, multivisited) = q.pop_front().unwrap();
        if node == End {
            n_paths += 1;
            continue;
        }

        let neighbours = graph.get(&node).unwrap();
        for n in neighbours {
            if (visited_small.contains(n) && multivisited) || n.clone() == Start { continue; }

            if let Small(_) | End = n {
                let mut vs = visited_small.clone();
                if visited_small.contains(n) && !multivisited && n.clone() != End {
                    q.push_back((n.clone(), vs, true));
                } else {
                    vs.insert(n.clone());
                    q.push_back((n.clone(), vs, multivisited));
                }
            } else {
                q.push_back((n.clone(), visited_small.clone(), multivisited));
            }
        }
    }

    n_paths
}

fn add_cave(graph: &mut HashMap<Cave, Vec<Cave>>, n1: Cave, n2: Cave) {
    if let Some(l) = graph.get_mut(&n1) {
        l.push(n2);
    } else {
        graph.insert(n1, vec![n2]);
    }
}
