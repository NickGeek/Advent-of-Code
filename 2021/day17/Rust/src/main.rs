// #[derive(Copy, Clone, Debug, Eq, PartialEq)]
// struct Velocity {
//     x:
// }

use std::collections::HashSet;

type Velocity = (isize, isize); // (x, y)

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct Probe {
    x: isize,
    y: isize,
    v: Velocity
}

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let target_str = raw.lines().next().unwrap();
    let target = target_str.split(": ")
        .skip(1)
        .take(1)
        .map(|s| {
            let (x_str, y_str) = s.split_once(", ").unwrap();
            let (_, x_range) = x_str.split_once("=").unwrap();
            let (x1_s, x2_s) = x_range.split_once("..").unwrap();

            let (_, y_range) = y_str.split_once("=").unwrap();
            let (y1_s, y2_s) = y_range.split_once("..").unwrap();

            ((x1_s.parse::<isize>().unwrap(), x2_s.parse::<isize>().unwrap()), (y1_s.parse::<isize>().unwrap(), y2_s.parse::<isize>().unwrap()))
        })
        .next().unwrap();

    part1(target);
    part2(target);
}

fn part1(target: ((isize, isize), (isize, isize))) {
    let ((x1, x2), (y1, y2)) = target;

    let mut highest_y = 0;
    let mut highest_y_v: Option<Velocity> = None;
    for v_x in 0..x2+300 {
        'outer: for v_y in y1..y2+300 {
            let mut probe = Probe {
                x: 0,
                y: 0,
                v: (v_x, v_y)
            };

            let mut highest_y_wk = 0;
            while !((probe.x >= x1 && probe.x <= x2) && (probe.y >= y1 && probe.y <= y2)) {
                if probe.y > highest_y_wk { highest_y_wk = probe.y; }
                step(&mut probe);

                // Is it possible?
                if probe.y < y1 && probe.v.1 < 0 {
                    // no, we're in a death spiral
                    continue 'outer;
                }
            }
            if highest_y_wk > highest_y {
                highest_y = highest_y_wk;
                highest_y_v = Some((v_x, v_y));
            }
        }
    }

    println!("Highest Y causing velocity: {:?}", highest_y_v);
    println!("Highest Y pos: {:?}", highest_y);

    fn step(p: &mut Probe) {
        p.x += p.v.0;
        p.y += p.v.1;
        if p.v.0 > 0 { p.v.0 -= 1 } else if p.v.0 < 0 { p.v.0 += 1 }; // drag
        p.v.1 -= 1; // gravity
    }
}

fn part2(target: ((isize, isize), (isize, isize))) {
    let ((x1, x2), (y1, y2)) = target;

    let mut initial_vs = HashSet::new();
    for v_x in 0..x2+300 {
        'outer: for v_y in y1..y2+300 {
            let mut probe = Probe {
                x: 0,
                y: 0,
                v: (v_x, v_y)
            };

            let mut highest_y_wk = 0;
            while !((probe.x >= x1 && probe.x <= x2) && (probe.y >= y1 && probe.y <= y2)) {
                if probe.y > highest_y_wk { highest_y_wk = probe.y; }
                step(&mut probe);

                // Is it possible?
                if probe.y < y1 && probe.v.1 < 0 {
                    // no, we're in a death spiral
                    continue 'outer;
                }
            }
            initial_vs.insert((v_x, v_y));
        }
    }

    println!("vs: {:?}", initial_vs.len());

    fn step(p: &mut Probe) {
        p.x += p.v.0;
        p.y += p.v.1;
        if p.v.0 > 0 { p.v.0 -= 1 } else if p.v.0 < 0 { p.v.0 += 1 }; // drag
        p.v.1 -= 1; // gravity
    }
}
