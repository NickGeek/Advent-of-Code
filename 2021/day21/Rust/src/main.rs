use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;
// use memoize::memoize;
use cached::proc_macro::cached;

fn main() {
    let p1_s = 8;
    let p2_s = 2;

    // let cache = Rc::new(RwLock::new(HashMap::new()));
    println!("{:?}", part2(p1_s, p2_s, 0, 0));

    // let cache = cache.read().unwrap();
    // let mut score1: u64 = 0;
    // let mut score2: u64 = 0;
    // for ((_, _, _, _), (s1, s2)) in &*cache {
    //     score1 += *s1;
    //     score2 += *s2;
    // }

    assert_eq!(track(1), 1);
    assert_eq!(track(12), 2);
    assert_eq!(track(10), 10);
    assert_eq!(track(11), 1);
    //
    // println!("{} and {}", score1, score2);
}

type CacheKey = (u64,u64,u64,u64);

#[cached]
fn part2(pos1: u64, pos2: u64, score1: u64, score2: u64) -> (u64, u64) {
    // {
    //     let cache = cache.read().unwrap();
    //
    //     if cache.contains_key(&(pos1, pos2, score1, score2)) {
    //         return;
    //     }
    // }

    let mut w1 = 0;
    let mut w2 = 0;

    for p1_roll in qdie() {
        let pos1 = track(pos1 + p1_roll);
        let score1 = score1 + pos1;


        if score1 >= 21 {
            w1 += 1;
            // (*cache.entry((pos1, pos2, score1, score2)).or_insert((1, 0))).0 += 1;\
        } else {
            for p2_roll in qdie() {
                let pos2 = track(pos2 + p2_roll);
                let score2 = score2 + pos2;
                if score2 >= 21 {
                    w2 += 1;
                } else {
                    let (s1, s2) = part2(pos1, pos2, score1, score2);
                    w1 += s1;
                    w2 += s2;
                }
            }
        }
    }

    (w1, w2)
}

fn track(pos: u64) -> u64 {
    let res = pos % 10;
    if res == 0 { 10 } else { res }
}

const fn qdie() -> [u64; 27] {
    [3,4,5,4,5,6,5,6,7,4,5,6,5,6,7,6,7,8,5,6,7,6,7,8,7,8,9]
}
