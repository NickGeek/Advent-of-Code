use std::collections::{HashMap, HashSet};
use std::fs;
use std::ops::Index;
use itertools::Itertools;

const PLAIN: [char; 7] = ['a', 'b', 'c', 'd', 'e', 'f', 'g'];

fn main() {
    let raw = fs::read_to_string("input.txt").unwrap();

    let res: u64 = raw.lines()
        .map(|line| {
            // let mut segs = HashMap::new();
            let (input, output) = line.split_once(" | ").unwrap();

            let mappings = {
                let mut m = HashMap::new();
                m.insert(pack("acedgfb"), 8);
                m.insert(pack("cdfbe"), 5);
                m.insert(pack("gcdfa"), 2);
                m.insert(pack("fbcad"), 3);
                m.insert(pack("dab"), 7);
                m.insert(pack("cefabd"), 9);
                m.insert(pack("cdfgeb"), 6);
                m.insert(pack("eafb"), 4);
                m.insert(pack("cagedb"), 0);
                m.insert(pack("ab"), 1);
                m
            };

            let mapping = PLAIN.iter()
                .permutations(PLAIN.len())
                .find(|perm| {
                    input.split(' ')
                        .map(|digit| generate_key(digit, &*perm))
                        .all(|key| mappings.contains_key(&*key))
                }).unwrap();

            output.split(' ')
                .map(|d|
                    d.chars()
                        .map(|c| PLAIN[mapping.iter().position(|k| **k == c).unwrap()])
                        .sorted()
                        .collect::<String>()
                )
                .map(|idx| (*mappings.get(&*idx).unwrap()).to_string())
                .collect::<String>()
                .parse::<u64>()
                .unwrap()
        })
        .sum();

    println!("{}", res);
}

fn pack(letters: &str) -> String {
    letters.chars().sorted().collect::<String>()
}

fn generate_key(digit: &str, perm: &[&char]) -> String {
    digit.chars()
        .map(|cell| PLAIN[perm.iter().position(|plain_cell| **plain_cell == cell).unwrap()])
        .sorted()
        .collect::<String>()
}
