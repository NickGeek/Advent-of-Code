use std::collections::VecDeque;
use itertools::Itertools;

fn main() {
    let raw = std::fs::read_to_string("inputU.txt").unwrap();
    let l = raw.lines().collect::<Vec<_>>();

    let len = l.len();
    l.into_iter()
        .map(part2)
        .sorted()
        .for_each(|a| println!("{:?}", a));
}

fn part2(line: &str) -> u64 {
    let mut ends = VecDeque::new();
    let mut goals = VecDeque::from([end(line.chars().nth(0).unwrap())]);
    parse_chunk(&mut ends, &mut goals, line.chars().skip(1).collect::<String>());
    let mut score = 0;
    println!("goals: {:?}", goals.iter().rev().collect::<String>());
    for ch in goals.iter().rev() {
        score *= 5;
        score += match ch {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => unreachable!()
        }
    }
    score
}

fn end(start: char) -> char {
    match start {
        '(' => ')',
        '{' => '}',
        '[' => ']',
        '<' => '>',
        _ => '0'
    }
}

fn start(end: char) -> char {
    match end {
        ')' => '(',
        '}' => '{',
        ']' => '[',
        '>' => '<',
        _ => '0'
    }
}

fn parse_chunk(ends: &mut VecDeque<char>, goals: &mut VecDeque<char>, chunk_p: String) {
    if goals.len() == 0 {
        return;
    }
    let goal = *goals.back().unwrap();

    for (i, ch) in chunk_p.chars().enumerate() {
        if ch == goal {
            goals.pop_back();
            let next = &chunk_p[i+1..];
            if next.len() == 0 { return; }
            return parse_chunk(ends, goals, next.to_string());
        }

        if end(ch) != '0' {
            goals.push_back(end(ch));
            return parse_chunk(ends, goals, chunk_p[i+1..].to_string());
        }
    }

    ends.push_back(goal);
    parse_chunk(ends, goals, format!("{}{}", chunk_p, ends.iter().collect::<String>()))
}
