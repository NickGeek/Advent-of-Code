use crate::Choice::{Draw, Loose, Win};
use crate::Item::{Paper, Rock, Scissors};

fn main() {
    p1(INPUT_T);
    println!("\n---------------\nfinal answer:");
    p1(INPUT);
    println!("\n---------------\np2:");
    p2(INPUT_T);
    println!("\n---------------\nfinal answer:");
    p2(INPUT);
}

fn p1(input: &str) {
    let mut a = input.lines()
        .map(|round| round.split(' ').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let res: u64 = a.iter()
        .map(|round| {
            let p1 = Item::from(round[0]); let p2 = Item::from(round[1]);
            let score_1 = score(p1, p2);
            let score_2 = score(p2, p1);
            println!("{}", score_2);
            (score_1, score_2)
        })
        .map(|(s1,s2)| s2)
        .sum();

    println!("{:?}", res);
}

fn p2(input: &str) {
    let mut a = input.lines()
        .map(|round| round.split(' ').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let res: u64 = a.iter()
        .map(|round| {
            let p1 = Item::from(round[0]);
            let choice = Choice::from(round[1]);

            for item in [Rock, Paper, Scissors] {
                let their_score = score(p1, item);
                let my_score = score(item, p1);
                match choice {
                    Loose => {
                        if my_score < their_score { return my_score; }
                        continue;
                    }
                    Draw => {
                        if my_score == their_score { return my_score; }
                        continue;
                    }
                    Win => {
                        if my_score > their_score { return my_score; }
                        continue;
                    }
                }
            }
            unreachable!()
        })
        .sum();

    println!("{:?}", res);
}

#[derive(Debug,Eq,PartialEq,Copy,Clone)]
enum Item {
    Rock,
    Paper,
    Scissors
}
impl From<&str> for Item {
    fn from(c: &str) -> Self {
        match c {
            "A" | "X" => Rock,
            "B" | "Y" => Paper,
            "C" | "Z" => Scissors,
            _ => unreachable!()
        }
    }
}

#[derive(Debug,Eq,PartialEq,Copy,Clone)]
enum Choice {
    Loose,
    Draw,
    Win
}
impl From<&str> for Choice {
    fn from(c: &str) -> Self {
        match c {
            "A" | "X" => Loose,
            "B" | "Y" => Draw,
            "C" | "Z" => Win,
            _ => unreachable!()
        }
    }
}

fn winner(me: Item, other: Item) -> bool {
    match me {
        Rock => other == Scissors,
        Paper => other == Rock,
        Scissors => other == Paper,
        _ => unreachable!()
    }
}

fn score(me: Item, other: Item) -> u64 {
    let mut score = match me {
        Rock => 1,
        Paper => 2,
        Scissors => 3,
        _ => unreachable!()
    };
    if me == other { score += 3 }
    else if winner(me, other) { score += 6; }
    score
}

const INPUT_T: &str = "A Y
B X
C Z";
const INPUT: &str = "A Y
B X
B X
C Y
B X
A Z
B X
B X
C Z
A Z
C Z
A X
A Y
A Y
B X
C Y
B Z
A X
B X
C Y
C Y
A Y
A Y
C Y
B X
B X
B X
A X
B X
B X
C Y
C Y
B X
A Z
A Z
B X
A Z
C Y
B X
B X
B X
B X
B X
B Z
A Z
B X
A Z
A X
B X
A Z
B X
C Y
A Z
A Z
B X
B X
B Z
C Y
A Y
B Z
A Z
C Z
B X
C Y
C Y
B Z
B X
B X
B Z
B X
B X
A Y
A Z
B Z
B X
B Y
B Z
B X
C X
B Z
B X
A Z
B Z
B Z
B X
A Y
A Z
A Y
B X
B Z
B X
B X
A Y
A Y
B X
B X
B X
C X
C Y
B X
B X
B Z
B X
B X
B X
C Y
B X
C Y
A Z
B X
B Z
B X
C Y
C Y
A Z
A Y
C Y
B X
B X
B Z
A Y
B X
B X
B X
A Y
B Z
B X
A Y
B Z
C Y
A X
B X
B X
A Z
C Y
B X
B Z
A Y
A Y
C Y
A Y
C Y
B X
B Z
B X
C Y
A Y
B X
C Z
A Z
C Y
B X
B Z
B X
B X
B Z
B Y
A Y
B X
A Y
A Z
A Z
B X
C Y
B Z
A X
A Y
B X
A Z
B X
A Y
B Y
B X
A Y
B X
B X
B X
B X
B X
B X
B X
A Z
B X
B X
B Z
B Z
B Z
B Z
A Y
B Z
C Z
B X
B X
B X
A Z
B Z
C Y
B X
C Y
B Z
B X
B Z
B X
C Z
A Z
A Y
A X
B X
B Z
A Y
B X
B X
B X
B X
C Y
C Y
A Z
A Y
A X
B X
B X
A Y
B X
C Y
A X
B X
B X
B X
A Z
C Y
B Z
C Y
A Y
C Y
A X
A Z
C Z
A Z
B X
B Z
B X
B X
A X
B X
B X
B Z
A Y
C Y
A Z
C Z
A Z
A Z
A Z
B Z
B Z
B Z
C Z
B Y
C Y
C Y
C X
B X
C Y
B X
B X
C Y
B Z
B X
C Z
B X
A Z
B Z
C X
C Y
B X
A Y
A Z
B X
A Z
A Z
B X
B X
C Z
B Z
A Z
A Z
C Z
B X
A Z
A X
B X
B X
C Y
B X
B Z
A Z
B X
B X
B X
B X
B X
B X
B X
B X
C Y
C Y
B X
B X
B X
C Y
A X
B Z
B X
B X
C Z
B X
B Z
B Z
A Z
B X
B Z
B X
A Z
B X
B X
B X
B X
B X
C Z
B X
B X
B X
B Z
C Y
C Y
A Y
B X
B Z
C Y
B X
B X
C Y
C Y
B X
B X
B X
B X
C X
B Z
B X
B Z
C Y
A X
A Z
B X
B X
A Z
B X
C Z
B X
A Y
A Z
B X
B X
B X
B Z
B X
A Y
C Y
B Z
B Z
C Y
B X
A Y
C Y
B X
C Z
B X
B X
A Z
B Z
B Z
B X
A Z
A Z
A Y
B Z
B X
B X
B X
B X
B Z
B X
B Z
B X
A Z
B X
B X
C Z
C Z
B X
C Y
A Z
B X
B Z
C Z
C X
B X
A Y
A Y
B X
B X
C Y
A X
C Y
A Y
A Z
C Z
B Z
A Z
B X
B X
B Z
C Y
A Y
A Y
B Z
A Z
A Z
B Z
A Z
B X
A Y
A Y
C Y
B X
B X
A X
B X
C X
A Y
A Z
B X
B X
B X
B X
B X
B Z
B Z
B Z
B X
B X
A Z
B X
C Z
A Z
C Y
B Z
C Y
A Y
C Y
B X
B X
B X
A Y
B Z
B Z
B X
B Z
B X
B X
B X
C Y
B X
B Z
A Z
B X
B X
B X
C Z
C Z
B Z
B X
A Z
A X
B X
B X
B Y
A Y
B X
B X
C Z
A Z
B X
B Z
B Z
A Y
A Z
B Z
B Z
A Y
A Y
A Z
C Y
C Y
B X
B X
A Z
B X
C Y
C X
A Z
B X
B X
B X
A Y
C Y
B Y
C Y
C X
A X
A Y
B Z
B X
B Z
A X
B X
C Z
B X
B X
A Y
B Z
B X
A Y
A Y
B X
C Y
B X
B X
C Y
B X
C Z
B Z
B X
A X
C Y
C Y
B X
B Z
B X
B X
A Z
B X
A Z
B Z
B X
B X
B Z
A Y
C Y
B Z
B X
A Y
B X
A X
C Z
B X
B Z
B Z
A X
C Y
A Z
B X
B Z
A Y
B X
C Y
A Y
B Z
C Y
B X
B X
B X
B X
B X
B X
C Y
B X
C Z
A Y
C Y
B X
C Y
B Z
B X
A X
B Z
B X
C X
B X
A Z
B X
A Y
B X
C Z
B X
B Z
A Y
B X
A Z
A Z
A Z
A Y
A Y
B X
A Z
B X
B X
A Y
A Y
A Y
B Z
C Y
B Z
B X
C Z
A Y
C Y
B X
B X
B Z
B X
B X
B X
C Y
C Y
A Y
B Z
B Z
B X
B X
B Z
B X
A Y
C Y
B X
C X
B Z
B X
B Z
B Z
C Y
B Z
B X
C X
B X
C Y
B X
A Z
B X
B X
B X
B X
B X
C Y
A Y
B X
C Y
B Z
B Z
B X
C X
B Z
C Y
C X
C Y
A Z
B Z
C Y
C X
B X
A Y
C Y
B X
C Z
B Z
A Y
C Z
C X
B X
B X
A Y
C X
B X
B X
A X
A Z
A X
A Y
A Y
C Y
B X
C Z
B X
C Y
A Y
B X
B X
B X
B X
A Z
C X
B X
B Z
B X
B X
C Y
B X
C Z
B X
B X
B X
B X
B X
C Y
B X
B X
A Y
B X
C Y
C Y
A Y
B X
A X
A Y
B X
B Z
B X
B Z
A X
A Y
A X
B X
C Z
A Y
B Z
A Y
B Z
B Z
B X
B Z
B X
B X
B X
B Z
B X
B X
B X
A Z
C Z
C Z
A X
A X
B X
B X
A X
B X
A X
A Z
B Z
B X
C Y
B X
C Y
A Y
C X
A X
A Y
B Z
B X
B X
C Z
B Y
A Y
B Y
C Y
A Y
B X
B X
C X
B Z
A Y
B Z
A Z
B X
B X
A Z
C Z
A Y
B X
B X
B Z
B X
B X
B Z
A X
A Z
C Y
A Y
A Z
B X
C Y
B Z
C Y
B X
B X
B Z
B X
A Y
B X
A Y
B X
A Y
B X
A Y
C Y
A Y
B Z
A Y
B X
A Z
C Y
B X
B X
B X
A X
B X
B Z
A Z
A Z
B X
B Z
B X
A Y
B Z
B X
B X
A Y
A Y
C Z
C Y
B X
A X
A Y
A X
C Y
B X
C X
B X
B X
A Y
A Y
A Y
B Y
B X
B X
B X
C Z
B Z
C Z
A X
B X
B X
A Y
A Z
B X
A Y
B Z
B Z
C Y
B X
B X
C X
B X
C X
B X
B Z
A X
B X
A Z
A X
B Z
A Y
B X
B X
B X
B X
B Z
A Z
B X
B Z
B X
B Z
C Y
B X
C Y
B X
B X
B X
A Y
B X
A Y
B X
B X
A X
C Z
C Y
A Z
B X
A Y
C Z
C Y
C Y
B Z
A Y
B X
A Y
B X
B X
B Z
B Z
B X
C X
B Z
A X
A Y
A Y
B Z
A Z
C Y
B X
B Z
A Y
C Y
A Y
A X
A Z
B X
B X
C Y
B X
C Y
B X
A X
B Z
C X
B Y
A Y
C Y
B X
A X
B X
B X
B X
A Z
A Y
B X
C Z
B X
C X
C Z
B X
B X
C Y
B X
B Z
A Y
A Y
B X
B X
B Z
C Y
B X
B X
B X
A Y
C Y
B X
A Y
A Z
C Y
B X
A Y
A Z
B Z
A Z
B X
C X
B Z
A Y
B Z
A Y
C Y
A X
A Y
B X
C Y
C Y
B X
A X
B Z
B X
B X
A Z
A Z
A Y
A Y
B X
B X
B X
A X
B X
B X
B X
A X
B Z
C Z
A Y
C Y
C Y
C X
B X
B Z
B Z
B X
B X
C Y
A X
C Y
A X
B X
B X
B Z
B Z
C Y
C Y
A Z
C Y
A Z
B X
A Z
A Y
B Z
B Z
B X
B X
A Y
B Z
B X
C Z
A Z
B Z
B X
B Z
B X
C Y
B X
A Y
B Z
C Y
B Z
C Z
B X
C Z
B X
B X
B X
C Y
B X
B X
B Z
A Y
B X
B Z
A Z
B X
C Y
C X
C Y
A Z
B X
A Z
B X
C Z
A Y
B Z
B Z
A Y
C Z
C X
A Y
B X
B Z
B X
B X
B X
B X
A X
B X
B X
A Z
B X
B X
A Z
C Y
A X
B Z
B X
B X
A Z
B X
A Y
B Z
C Y
B X
A Y
C Y
B Z
B X
A Z
A Z
B X
B X
B Z
B X
B X
A Z
B X
B Z
B Z
C Y
A Z
B X
A Y
B X
B X
B X
B Z
A X
C Y
B Z
B X
B X
B X
B X
B X
B Z
B X
B Z
A Z
B Z
B X
C Z
B Z
C Z
A Z
C X
A Y
B Z
B Z
B X
B Z
B Z
A Z
C Z
A X
C Y
C Y
A Y
A X
A Z
B X
A Y
B X
B X
C Y
C Y
B X
B Z
A Y
B X
B Z
B X
B X
A Z
C Y
B X
C Y
A Y
B Z
A Z
A Y
B X
A Z
B Z
B X
A Y
A Y
A Z
A X
B Z
C Z
B X
C Z
B X
B Z
A X
B X
B X
B X
B X
C Y
B X
A X
B Z
A Y
B X
A Z
B X
B Z
A Y
A Y
A Y
A Y
A Z
C Y
C Z
B X
B Z
A Y
B X
B X
B X
A Y
B Z
B X
B Z
B X
B X
B X
C Y
C Y
C X
B X
A Y
A Y
C Z
A Y
A Y
C X
A Z
B X
B X
B X
B X
C Y
A Y
B Z
A Z
B X
C X
A Z
B X
C Y
B X
B X
C Y
B X
B X
B X
A Y
B X
B X
C Z
B X
C Y
C Y
B X
A Z
B X
B X
C Y
A Y
B X
B Z
B X
B X
B X
C Y
C Z
B Z
C Y
B X
B Z
B X
B X
B X
C Y
C X
B X
A Y
A Z
B Z
A Y
B X
B X
B Y
C Y
C X
B Z
A Z
B X
B X
A Y
C Z
C Y
B X
C Y
A Z
A Y
B Z
B X
B X
B X
B X
A Z
A X
A X
A X
C Y
B Z
B X
C Y
B X
B X
A Z
C Z
C Z
B X
A Y
B X
B Z
B X
A Y
A Y
A Z
C Y
A Y
A Z
A Y
A Y
B X
A Y
B X
A Z
A Z
C Y
B X
B X
C Y
B X
B X
B X
C Y
A Y
B X
A Y
B Z
C Y
B X
A Y
B Z
A Z
B Z
A Z
B X
C Y
B X
B X
B X
C Y
A Y
B X
A Y
C X
A Z
B X
B Y
B X
B X
A Z
B X
B X
B Z
B X
C Y
B X
B X
B Z
B X
C Y
B X
C Y
B X
B Z
A Y
B X
B X
C Z
B X
C Y
A Y
C Y
B X
B Z
B X
B X
B X
B X
A Y
B X
A Z
A Y
A X
C Y
B X
C Y
B X
A Z
C Y
C Y
B Z
B X
A Y
B Z
A Y
A Y
B Z
B Z
B X
C Y
C Y
B X
A X
A Z
B X
C Y
B X
B X
B Z
B X
B Z
C Y
C Z
C Y
A Y
A Z
B X
C Y
A Y
C X
B X
B X
A Y
B X
B X
B X
B Z
B Y
A X
A Y
C Y
C Y
A Y
C Y
B Z
B Z
C Y
B X
C Y
C Z
A Y
C X
A Y
A Y
C Y
C Y
B X
A Y
A X
B X
B X
B Z
B X
A Z
A Y
A Y
B X
B X
B Z
B Z
B X
B Z
A Z
A Y
A Z
B X
C Y
B Z
B X
A Y
B X
C X
B X
B X
C Y
B X
B X
B X
A Y
A X
A Y
B X
B X
A Y
A Y
A Z
A Z
C X
C Y
A Z
C Y
B Z
B X
A Y
B X
B Z
C Z
A Y
A Z
A Y
B X
B Z
B X
A X
B Z
B X
C Y
B X
A Z
C Z
A X
B X
B X
A Y
C Y
B X
B X
B X
B X
B X
A X
B X
A Z
C Z
B Z
B X
C X
A X
C Y
A Y
A Z
B X
A X
A Z
A Z
B X
B X
A Z
C Y
A Z
B X
C Y
B X
C Z
C Y
C X
A Z
C X
B Z
B X
B X
C Y
B X
C Y
A Z
A Z
A Z
A Y
A Z
B X
B X
B X
B Z
C Y
C Z
C Y
C Y
C Y
A Y
B Z
A X
B X
B X
B X
A X
A Y
B X
B Z
A Z
A Y
A Y
B Z
B X
B X
C Y
C Y
C Y
B X
C Y
B X
B X
B X
C Y
A Y
A Y
C Z
C Y
B X
A X
A Y
B X
C Y
B X
A Y
C Y
A Y
B X
B X
B Z
A Y
B Z
B Z
B X
B X
C X
C Y
B X
B X
A X
C Y
B X
B Z
B X
B X
A Y
B X
B Z
B Z
C Y
B X
B Z
B Z
B Z
B X
B X
A Y
B X
B X
B Z
C Y
A Y
B X
B Z
B X
B X
A X
B X
B X
B X
B Z
B Z
B Z
B X
A Z
B X
A Z
A Y
B Z
B X
A Y
B X
B Z
B X
A Z
B X
B Z
C Z
A X
B X
B X
B X
C Y
B X
B X
A Y
A Z
B X
C Y
B X
C Y
B X
B Y
B X
C Z
B X
C Z
A Y
B X
C Z
B X
A Y
C Y
B X
B X
B Z
C X
B Z
B X
A Z
B X
B X
B X
B Z
B X
A Z
C Y
A Y
B X
B X
A Z
B X
A Y
B X
C Y
B Z
B Z
B X
B Z
B X
B X
B X
B X
B X
C Y
A Z
A Z
B Z
A Y
B Z
C Y
A Y
A Y
C Y
A Y
B Z
A Y
A Y
B X
B X
A X
B X
C Y
C Y
B Z
B X
A Z
B Z
B X
A Z
B Z
A Y
A Y
A Y
A Y
A Z
A Y
B X
B Z
A Y
B X
A Z
B Z
C X
B X
B X
B Z
B Z
B X
B X
B X
B X
B X
B X
B X
B Z
B X
B X
B X
B X
B X
B Z
B X
B X
B Z
B Z
B X
A Y
B Z
B X
A Z
A Y
A Z
B X
B X
A X
B Y
A Z
B X
C Z
A Z
B X
B Z
B X
B X
B X
A Y
C Y
B X
B X
B X
A Y
B X
A Z
B X
B X
B X
B Z
B Z
C X
C Y
B X
B X
C Y
B X
B X
B X
A X
A Z
A Z
B Z
B Z
B X
A Y
B X
C Y
B Z
C Y
C Z
A Y
A Y
B X
B X
A Z
B X
B X
B Z
B X
B X
A Z
B X
B X
B X
C Y
B X
B X
C Y
B X
B X
B Z
B X
B X
A Y
C Y
A X
C Y
B X
B X
A Z
A Y
B X
A X
B Z
A Y
B X
B X
A Y
B X
C X
B X
B X
B Z
B Z
B X
B Z
B X
B X
A Y
A Y
B X
B Z
C Y
B X
B X
B X
A Z
B X
A Y
B X
C Y
A X
A Y
B X
B X
C Y
B X
B X
B X
A Z
B Z
B X
C X
A Z
C Y
B X
B Z
B X
B X
A Z
B X
A Y
C Y
B X
B Z
C X
A Z
A Z
B Z
B Z
A X
B X
A Y
B X
B X
B X
B X
A Z
B Z
B Z
C Y
A X
A Y
A Y
A Z
B X
A Y
B Z
C Y
B Z
B X
A Z
B Z
B X
B Z
B Z
B Z
B X
C Y
B X
B X
A Y
B X
C X
A Y
A Z
A Y
B X
C Y
B Z
B X
B X
A X
B X
B X
A Y
A Z
C Z
A Y
B X
B X
C Y
A X
A Z
B X
B Z
B Z
C Y
A Z
C Y
A Z
C Y
C Y
B Z
C Y
B X
C Y
C Y
B X
B Z
C X
C Y
A Y
B X
C Y
B X
B X
C Y
B Z
B Z
A Z
A Z
C Y
B X
B Z
B X
B X
B X
C Y
B X
C Y
A Z
A Z
B X
B X
A Y
A X
B X
A Z
A Y
C Y
B X
A Y
A Y
B X
A X
A Z
A Y
A Z
C Y
C Y
C Y
A Y
B X
B X
A Y
C Y
B X
A X
C Y
C Y
A Y
C Y
C Y
B Z
C Y
B X
C Y
B X
B X
B X
A Y
B Y
B X
C X
B X
A X
A Y
C Y
C Y
B X
B X
A Y
B X
B X
A Y
C Y
C X
A Y
B X
B Z
B Z
B Z
B X
A Z
B X
B X
A Y
B X
B X
A Y
B Z
A Y
B X
B X
B Z
A Z
A Y
A Z
A Y
C Y
B X
C X
B X
B X
A Z
B X
B Z
B X
A Z
C Y
B X
B X
B Z
A Z
A Y
B X
B Y
A Y
A Z
A Y
C Z
B X
B X
B X
A Z
B X
B X
B X
A Y
B Z
C Y
C Y
A Z
A Z
A Y
B X
A Y
B Z
C Y
B X
B Z
A Y
B X
A Y
C Y
B X
B X
A Y
A Z
A Z
A Z
B X
B X
A Z
B X
B X
B Z
A X
B Z
A Z
B X
C Y
C X
A Z
B X
A Y
B Y
B Z
B X
B X
A Y
B X
A Z
A Z
C X
B X
B Z
A Y
C Y
A Y
B X
B Z
B X
A Z
B Z
C Y
B Z
B X
B X
B X
A Y
B X
B X
A Z
C Y
B X
C Y
B X
C Y
A Z
A Y
A X
C Y
B X
B X
B Z
B X
A Y
A Y
B X
B X
A Z
C Y
B Z
B X
B X
A X
A Y
B Z
C Y
A Z
C Y
A Z
B X
A Y
B X
A Y
B X
B X
C X
B Y
C Z
A Z
C Y
B X
B X
B Z
C Z
A X
A Y
A Z
B X
C Y
B Z
B Z
A Y
B X
B X
A Y
A Z
B X
B X
A Z
B X
B X
B Z
A X
B X
A X
A Y
C Y
B X
B Z
A Y
B X
A X
B X
B X
B Z
B X
B X
A Y
B X
B X
A Y
A Y
C Y
B Z
B X
A X
A Z
B X
B X
C Z
B X
B X
B X
B X
B X
C Y
C Y
C X
B X
B X
B Z
B Z
B X
A Y
B Z
B Z
B Z
B X
B X
B X
B X
A Y
A Y
B X
C Y
C Y
B X
B X
A Y
C Y
A Z
C Y
B X
C Z
B X
B Z
B X
C Y
C Y
B X
B X
B X
C Y
B X
B X
A X
B X
C Y
A Y
B X
B X
B X
C X
C Y
A Y
B X
B X
C Y
A Y
B X
A Y
A Y
B Z
C Y
B Z
A Y
A Z
C Y
B X
C Y
B X
C Y
C Y
B X
B X
B X
B Z
A Y
B X
B X
B X
A Y
B X
B X
A Y
B X
A Y
C Y
A Z
B X
B X
C Y
B X
B X
A Y
A Z
C Y
B X
B Z
B Z
B X
C Y
B X
A Y
C X
C Y
B X
B X
B Z
C Y
B X
B X
C Y
C Y
B X
C Y
B Z
C Y
C X
B Z
C Y
B Z
C Y
A X
B Z
B Z
B X
B X
C Y
B X
C Y
B X
B X
B X
A Y
B X
C Y
B Z
C Y
C Y
A Y
C Z
A Y
B X
A Z
C Z
C Y
B X
B X
A Y
B Z
B X
C Y
B X
B Z
A Z
B Z
A Y
C Y
A Z
B X
B X
C Y
C Y
B X
B X";
