use std::collections::HashMap;
use itertools::Itertools;

type Pairs = HashMap<(char, char), u64>;

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();

    let (template, rules) = raw.split_once("\n\n").unwrap();

    let rules = rules.lines()
        .map(|rule| rule.split_once(" -> ").unwrap())
        .map(|(from, to)| {
            let chs = from.chars().collect_vec();
            ((chs[0], chs[1]), to.chars().exactly_one().unwrap())
        })
        .collect::<HashMap<_, _>>();

    let chars = template.chars().collect::<Vec<_>>();
    let mut pairs = HashMap::<(char, char), u64>::new();
    let mut last = '\0';
    for i in 0..(chars.len() - 1) {
        let pair = (chars[i], chars[i + 1]);
        let old = if let Some(count) = pairs.get(&pair) { *count } else { 0 };
        pairs.insert(pair, old + 1);
        last = chars[i + 1];
    }

    let mut res = pairs;
    for _ in 0..40 {
        res = step(&res, &rules);
    }

    let mut commons = HashMap::new();
    for ((a, b), count) in res {
        let old = if let Some(count) = commons.get(&a) { *count } else { 0 };
        commons.insert(a, old + count);
        last = b;
    }
    let old = if let Some(count) = commons.get(&last) { *count } else { 0 };
    commons.insert(last, old + 1);

    let (min_ch, min_count) = commons.iter().min_by_key(|(ch, count)| **count).unwrap();
    let (max_ch, max_count) = commons.iter().max_by_key(|(ch, count)| **count).unwrap();

    println!("{:?}", commons);
    println!("{:?}", max_count - min_count);
}

fn step(pairs: &Pairs, rules: &HashMap<(char, char), char>) -> Pairs {
    let mut res = HashMap::new();

    for (pair, total) in pairs {
        if let Some(to) = rules.get(pair) {
            let (a, b) = *pair;
            let total = *total;
            let to = *to;

            let new_p_l = (a, to);
            let old_l = if let Some(count) = res.get(&new_p_l) { *count } else { 0 };
            let new_p_r = (to, b);
            let old_r = if let Some(count) = res.get(&new_p_r) { *count } else { 0 };
            res.insert(new_p_l, old_l + total);
            res.insert(new_p_r, old_r + total);
        } else {
            panic!("at the disco")
        }
    }

    res
}
