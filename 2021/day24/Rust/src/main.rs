use std::cell::{Cell, RefCell};
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use rayon::prelude::*;
use itertools::Itertools;

/*
    1.
    2.
    3.
    4.
    5.
    6.
    7.
    8.
    9.
    10.
    11.
    12.
    13. for z = w - 21, z%26 - 4
    14. for z = 0, y = 0. for y = 0, x == w. for x == w, z == w - 21
 */

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let opt = std::fs::read_to_string("opt.txt").unwrap();

    let mut blocks = vec![];
    blocks.push(vec![]);
    let mut group_n = 0;
    for line in raw.lines() {
        if line.starts_with("inp") {
            blocks.push(vec![]);
            group_n = blocks.len() - 1;
        }
        blocks[group_n].push(line);
    }

    let mut input = "13579246899999".chars().collect::<VecDeque<_>>();
    let res = run(&*raw);
    println!("res: {:?}", res);

    // (8999999999999i128..=99999999999999i128).into_par_iter()
    //     .for_each(|i| {
    //         let input = i.to_string();
    //         if input.contains('0') { return; }
    //         let res = run(input.chars().collect::<VecDeque<_>>(), &*raw);
    //
    //         if let Some(z) = res.get("z") {
    //             if z == &0 {
    //                 println!("{}", i);
    //             }
    //         }
    //     });

    // println!("Test bed: ");
    // // let input = 8999999999999i128.to_string();
    // let input = "27158493799998i128".chars().collect::<VecDeque<_>>();
    // // let res = run(input.clone(), &*raw);
    // let res2 = run(input.clone(), &*opt);
    // // assert_eq!(res, res2);
    //
    // let mut res = String::new();
    // let a = std::io::stdin().read_line(&mut res);
    //
    // if let Some(z) = res2.get("z") {
    //     println!("{:?}", res2);
    //     // if z == &0 {
    //     // }
    // }

    // println!("{}", compile(&*raw));
}

// BFS-esque approach inspired by https://github.com/Mahrgell/AoC2021/blob/main/aoc21-24/src/main.rs
fn run(code: &str) -> Option<u64> {
    // Brute force the register states
    let mut mems = Vec::new();
    mems.push((Memory::default(), 0));

    for line in code.lines() {
        if line.starts_with("inp") {
            let mut visited = Rc::new(RefCell::new(HashMap::new()));
            let mut new_mems = Vec::with_capacity(mems.len()); // >= mems.len()

            for (mem, input) in mems {
                for i in 1u8..=9u8 {
                    let mut mem = mem.clone();
                    compute(line, &mut mem, Some(i as i64));

                    let input = input * 10 + (i as u64);

                    let visited = visited.clone();
                    let mut visited = (&*visited).borrow_mut();
                    if let Some(idx) = visited.get(&mem) {
                        let prev: &mut (Memory, u64) = new_mems.get_mut(*idx).unwrap();
                        prev.1 = min(input, prev.1);
                        continue;
                    }

                    visited.insert(mem, new_mems.len());
                    new_mems.push((mem, input));
                }
            }

            mems = new_mems;
            println!("wow {}", mems.len());
        } else {
            mems.par_iter_mut().for_each(|(mem, _)| compute(line, mem, None));
        }
    }

    mems.into_par_iter()
        .filter(|(mem, _)| mem.z == 0)
        .map(|(_, input)| input)
        .min()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Memory {
    w: i64,
    x: i64,
    y: i64,
    z: i64
}
impl Memory {
    fn mut_handle(&mut self, ident: &str) -> &mut i64 {
        match ident {
            "w" => &mut self.w,
            "x" => &mut self.x,
            "y" => &mut self.y,
            "z" => &mut self.z,
            _ => panic!()
        }
    }

    fn handle(&self, ident: &str) -> &i64 {
        match ident {
            "w" => &self.w,
            "x" => &self.x,
            "y" => &self.y,
            "z" => &self.z,
            _ => panic!()
        }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            w: 0,
            x: 0,
            y: 0,
            z: 0,
        }
    }
}

fn compute(instruction: &str, mem: &mut Memory, input: Option<i64>) {
    let command = instruction.split(' ').collect::<Vec<_>>();
    let ins = command[0];
    let a = command[1];
    let a_deref = match a.parse::<i64>() {
        Ok(a) => a,
        Err(_) => *mem.handle(a)
    };

    let mut b = 0;
    if command.len() >= 3 {
        let b_i = command[2];
        b = match b_i.parse::<i64>() {
            Ok(b) => b,
            Err(_) => *mem.handle(b_i)
        };
    }

    let recv = mem.mut_handle(a);

    match ins {
        "inp" => {
            *recv = input.expect("Must provide input to execute `inp` instruction.")
        },
        "add" => *recv = a_deref + b,
        "mul" => *recv = a_deref * b,
        "div" => *recv = a_deref / b,
        "mod" => *recv = a_deref % b,
        "eql" => *recv = if a_deref == b { 1 } else { 0 },
        _ => unimplemented!()
    };
}
