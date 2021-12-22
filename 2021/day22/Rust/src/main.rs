use std::cmp::{max, min};
use std::collections::HashSet;
use bbox::BoundingBox;
use nalgebra::Point3;

type Pos = (i64, i64, i64);

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();

    // nvm I'm gonna do part 1 stupidly
    let mut points = HashSet::new();
    for l in raw.lines() {
        let (status, cons) = l.split_once(" ").unwrap();
        let ranges = cons.split(',')
            .map(|range| {
                let (from, to) = (&range[2..]).split_once("..").unwrap();
                (from.parse::<i64>().unwrap(), to.parse::<i64>().unwrap())
            })
            .collect::<Vec<_>>();

        let from = (ranges[0].0, ranges[1].0, ranges[2].0);
        let to = (ranges[0].1, ranges[1].1, ranges[2].1);
        for x in max(from.0, -50)..=min(to.0, 50) {
            for y in max(from.1, -50)..=min(to.1, 50) {
                for z in max(from.2, -50)..=min(to.2, 50) {
                    if status == "on" {
                        points.insert((x, y, z));
                    } else {
                        points.remove(&(x, y, z));
                    }
                }
            }
        }
    }

    println!("{:?}", points.len());

    // Part 2
    let mut on_state = Vec::<(Pos, Pos)>::new();
    let mut off_state = Vec::<(Pos, Pos)>::new();
    let mut vol = 0;
    for l in raw.lines() {
        let (status, cons) = l.split_once(" ").unwrap();
        let ranges = cons.split(',')
            .map(|range| {
                let (from, to) = (&range[2..]).split_once("..").unwrap();
                (from.parse::<i64>().unwrap(), to.parse::<i64>().unwrap())
            })
            .collect::<Vec<_>>();

        let from = (ranges[0].0, ranges[1].0, ranges[2].0);
        let to = (ranges[0].1, ranges[1].1, ranges[2].1);

        let mut new_on_state = on_state.clone();
        let mut new_off_state = off_state.clone();

        if status == "on" {
            vol += volume((from, to));
            new_on_state.push((from, to));
        }

        let idxs = intersects(&(from, to), &on_state);
        for idx in idxs {
            let other = on_state[idx];
            let itx = intersection_pt((from, to), other);
            vol -= volume(itx);
            new_off_state.push(itx);
        }

        let idxs = intersects(&(from, to), &off_state);
        for idx in idxs {
            let other = off_state[idx];
            let itx = intersection_pt((from, to), other);
            vol += volume(itx);
            new_on_state.push(itx);
        }

        on_state = new_on_state;
        off_state = new_off_state;
    }
    println!("{:?}", on_state);
    println!("{:?}", off_state);
    println!("{}", vol);
}

fn add_cuboid((from, to): (Pos, Pos), state: &mut Vec<(Pos, Pos)>) {
    let idxs = intersects(&(from, to), &state);
    if !idxs.is_empty() {
        for idx in idxs {
            // Expand!
            let (other_f, other_t) = state[idx];
            let min_x = if from.0 < other_f.0 { from.0 } else { other_f.0 };
            let max_x = if to.0 > other_t.0 { to.0 } else { other_t.0 };
            let min_y = if from.1 < other_f.1 { from.1 } else { other_f.1 };
            let max_y = if to.1 > other_t.1 { to.1 } else { other_t.1 };
            let min_z = if from.2 < other_f.2 { from.2 } else { other_f.2 };
            let max_z = if to.2 > other_t.2 { to.2 } else { other_t.2 };

            state[idx] = ((min_x, min_y, min_z), (max_x, max_y, max_z));
        }
    } else {
        state.push((from, to));
    }
}

fn intersects(range: &(Pos, Pos), state: &Vec<(Pos, Pos)>) -> Vec<usize> {
    state.iter().enumerate()
        .filter(|(i, other)| {
            let x = range.0.0 <= other.1.0 && range.1.0 >= other.0.0;
            let y = range.0.1 <= other.1.1 && range.1.1 >= other.0.1;
            let z = range.0.2 <= other.1.2 && range.1.2 >= other.0.2;

            x && y && z
        })
        .map(|(i, _)| i)
        .collect()
}

fn intersection_pt(range: (Pos, Pos), other: (Pos, Pos)) -> (Pos, Pos) {
    let from_r = Point3::from([range.0.0 as f64, range.0.1 as f64, range.0.2 as f64]);
    let to_r = Point3::from([range.1.0 as f64, range.1.1 as f64, range.1.2 as f64]);

    let from_o = Point3::from([other.0.0 as f64, other.0.1 as f64, other.0.2 as f64]);
    let to_o = Point3::from([other.1.0 as f64, other.1.1 as f64, other.1.2 as f64]);

    let bb_r = BoundingBox::new(&from_r, &to_r);
    let bb_o = BoundingBox::new(&from_o, &to_o);

    let i = bb_o.intersection(&bb_r);

    let min = (i.min.x as i64, i.min.y as i64, i.min.z as i64);
    let max = (i.max.x as i64, i.max.y as i64, i.max.z as i64);

    (min, max)
}

fn volume(cuboid: (Pos, Pos)) -> i64 {
    let length = (cuboid.1.0 - fix(cuboid.0.0)).abs();
    let height = (cuboid.1.1 - fix(cuboid.0.1)).abs();
    let depth = (cuboid.1.2 - fix(cuboid.0.2)).abs();
    length * height * depth
}

fn fix(a: i64) -> i64 {
    if a == 0 { 0 } else { a - 1 }
}
