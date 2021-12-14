use std::collections::HashMap;
use itertools::Itertools;

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();

    let (template, rules) = raw.split_once("\n\n").unwrap();
    let rules = rules.lines()
        .map(|rule| rule.split_once(" -> ").unwrap())
        .map(|(from, to)| (from.to_string(), to.to_string()))
        .collect::<HashMap<String, String>>();

    let mut res = template.to_string();
    for _ in 0..40 {
        res = step(&*res, &rules);
    }

    let mut counts = HashMap::new();
    for ch in res.chars() {
        let old = if let Some(count) = counts.get(&ch) { *count } else { 0 };
        counts.insert(ch, old + 1);
    }

    let (min_ch, min_count) = counts.iter().min_by_key(|(ch, count)| **count).unwrap();
    let (max_ch, max_count) = counts.iter().max_by_key(|(ch, count)| **count).unwrap();

    // println!("{:?}", res);
    println!("{:?}", max_count - min_count);
}

fn step(poly: &str, rules: &HashMap<String, String>) -> String {
    let mut res = String::from(poly);

    let chars = poly.chars().collect::<Vec<_>>();
    let mut offset = 0;
    for i in 0..(chars.len() - 1) {
        let pair = format!("{}{}", chars[i], chars[i + 1]);
        if let Some(to) = rules.get(&*pair) {
            res.insert_str(i + 1 + offset, &*to);
            offset += 1;
            // res.push_str(&*to);
        }
    }
    // for (from, to) in rules {
    //     if poly.contains(&*from) {
    //         res.push_str(&*to);
    //     }
    // }

    res
}
