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

        if let Some(l) = adj_list.get_mut(&n1) {
            l.push(n2.clone());
        } else {
            adj_list.insert(n1.clone(), vec![n2.clone()]);
        }

        if let Some(l) = adj_list.get_mut(&n2) {
            l.push(n1.clone());
        } else {
            adj_list.insert(n2.clone(), vec![n1.clone()]);
        }
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
