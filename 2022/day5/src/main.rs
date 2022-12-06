use std::collections::VecDeque;
use std::fs;
use itertools::Itertools;
use regex::Regex;

fn main() {
    p1(&*fs::read_to_string("input_t.txt").unwrap());
    println!("\n---------------\nfinal answer:");
    p1(&*fs::read_to_string("input.txt").unwrap());
    println!("\n---------------\np2:");
    p2(&*fs::read_to_string("input_t.txt").unwrap());
    println!("\n---------------\nfinal answer:");
    p2(&*fs::read_to_string("input.txt").unwrap());
}

#[derive(Copy, Clone, Debug)]
enum Item {
    Exists(char),
    Empty
}
impl From<&str> for Item {
    fn from(ch: &str) -> Self {
        if ch.starts_with('[') {
            Item::Exists(ch.chars().nth(1).unwrap())
        } else {
            Item::Empty
        }
    }
}

fn p1(input: &str) {
    let mut cols: Vec<VecDeque<Item>> = Vec::new();
    input.lines().enumerate()
        .take_while(|(_,l)| !l.starts_with("move"))
        .for_each(|(i,line)| line.chars().map(|c| match c {
            '#' => Item::Empty,
            c => Item::Exists(c)
        }).enumerate().for_each((|(col, item)| {
            if cols.len() <= col { cols.push(VecDeque::new()); }
            let row = cols.get_mut(col).unwrap();
            row.push_back(item);
        })));

    let move_regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    input.lines()
        .skip_while(|l| !l.starts_with("move"))
        .for_each(|l| {
            let mov = move_regex.captures(l).unwrap();
            let quantity = mov.get(1).unwrap().as_str().parse::<usize>().unwrap();
            let from = mov.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1;
            let to = mov.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1;
            for _ in 0..quantity {
                // println!("{}\n{} {} {}\n{:?}", l, quantity, from, to, cols);
                move_item(&mut cols, from, to);
            }
        });

    let res = cols.iter()
        .map(|c| c.get(0).unwrap_or(&Item::Empty))
        .map(|it| match it {
            Item::Exists(c) => *c,
            Item::Empty => ' '
        })
        .collect::<String>();

    println!("{:?}", res);
}

fn move_item(cols: &mut Vec<VecDeque<Item>>, from: usize, to: usize) {
    if let Some(item) = cols[from].pop_front() {
        match item {
            Item::Exists(_) => cols[to].push_front(item),
            Item::Empty => move_item(cols, from, to)
        }
    } else {
        // panic!("Empty at {} to {}\n{:?}\n{:?}", from, to, cols[from], cols[to]);
    }
}

fn move_item2(quantity: usize, cols: &mut Vec<VecDeque<Item>>, from: usize, to: usize) {
    if quantity == 1 {
        move_item(cols, from, to);
        return;
    }
    let mut to_move = vec![];
    let mut dyn_quant = quantity;
    for _ in 0..dyn_quant {
        if let Some(item) = cols[from].pop_front() {
            match item {
                Item::Exists(_) => to_move.push(item),
                Item::Empty => dyn_quant += 1
            }
        } else {
            dyn_quant += 1
            // panic!("Empty at {} to {}\n{:?}\n{:?}", from, to, cols[from], cols[to]);
        }
    }
    for it in (0..to_move.len()).rev() {
        cols[to].push_front(to_move[it]);
    }
}

fn p2(input: &str) {
    let mut cols: Vec<VecDeque<Item>> = Vec::new();
    input.lines().enumerate()
        .take_while(|(_,l)| !l.starts_with("move"))
        .for_each(|(i,line)| line.chars().map(|c| match c {
            '#' => Item::Empty,
            c => Item::Exists(c)
        }).enumerate().for_each((|(col, item)| {
            if cols.len() <= col { cols.push(VecDeque::new()); }
            let row = cols.get_mut(col).unwrap();
            if let Item::Exists(_) = item {
                row.push_back(item)
            }
        })));

    let move_regex = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    input.lines()
        .skip_while(|l| !l.starts_with("move"))
        .for_each(|l| {
            let mov = move_regex.captures(l).unwrap();
            let quantity = mov.get(1).unwrap().as_str().parse::<usize>().unwrap();
            let from = mov.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1;
            let to = mov.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1;
            move_item2(quantity, &mut cols, from, to);
        });

    let res = cols.iter()
        .map(|c| c.get(0).unwrap_or(&Item::Empty))
        .map(|it| match it {
            Item::Exists(c) => *c,
            Item::Empty => ' '
        })
        .collect::<String>();

    println!("{:?}", res);
}
