use std::collections::{HashMap, HashSet, VecDeque};
use rayon::prelude::*;
use itertools::Itertools;

type V3 = (i64, i64, i64);

fn main() {
    let raw = std::fs::read_to_string("./input.txt").unwrap();
    let rs = raw.split("\n\n")
        .map(|readings| {
            let mut rs = readings.lines();
            let header = rs.next().unwrap();
            let header = (&header[12..header.len() - 4]).parse::<usize>().unwrap();

            let rs = rs
                .map(|reading| -> V3 {
                    let r = reading
                        .splitn(3, ",")
                        .map(|r| r.parse::<i64>().unwrap())
                        .collect::<Vec<_>>();
                    (r[0], r[1], r[2])
                })
                .collect::<HashSet<_>>();
            rs
        })
        .collect::<Vec<_>>();

    let mut offsets = HashMap::with_capacity(rs.len());
    for (i, a) in rs.iter().enumerate() {
        for (j, b) in rs.iter().enumerate() {
            if i == j && i != 0 { continue; }
            if let Some((offset, rotation_idx)) = get_offset_and_rotate(b, a) {
                offsets.entry(i).or_insert(HashMap::new()).insert(j, (offset, rotation_idx));
            }
        }
    }
    assert_eq!(offsets.len(), rs.len());

    // Now make everything relative to 0
    let mut zeroed_beacons = HashMap::with_capacity(rs.len());
    for (sensor, data) in offsets.iter() {
        let sensor = *sensor;
        let mut visited = HashSet::new();
        let mut q = VecDeque::new();
        for (relative_to, (offset, rotation_idx)) in data {
            q.push_back((relative_to, offset, rotation_idx, rs[sensor].clone()));
        }
        while !q.is_empty() {
            let (relative_to, offset, rotation_idx, r) = q.pop_front().unwrap().clone();
            if *relative_to == 0 {
                let res: HashSet<V3> = r.iter()
                    .map(|x| rotations(x)[*rotation_idx])
                    .map(|x| apply_offset(&x, offset))
                    .collect();

                zeroed_beacons.insert(sensor, res);
                break;
            }

            let relative_data: HashSet<V3> = r.iter()
                .map(|x| rotations(x)[*rotation_idx])
                .map(|x| apply_offset(&x, offset))
                .collect();

            visited.insert(relative_to);
            for (relative_to, (offset, rotation_idx)) in offsets.get(relative_to).unwrap() {
                if !visited.contains(relative_to) {
                    q.push_back((relative_to, offset, rotation_idx, relative_data.clone()));
                }
            }
        }
    }

    assert_eq!(zeroed_beacons.len(), rs.len());

    let bs = zeroed_beacons.values().flat_map(|v| v.iter()).collect::<HashSet<_>>();
    println!("{:?}", bs.len());


    // Part 2
    let mut zeroed_offsets = HashMap::with_capacity(rs.len());
    for (sensor, data) in offsets.iter() {
        let mut visited = HashSet::new();
        let mut q = VecDeque::new();
        for (relative_to, (offset, rotation_idx)) in data {
            q.push_back((relative_to, offset, rotation_idx, (0, 0, 0)));
        }
        while !q.is_empty() {
            let (relative_to, offset, rotation_idx, zero) = q.pop_front().unwrap().clone();
            if *relative_to == 0 {
                let r_zero = apply_offset(&rotations(&zero)[*rotation_idx], offset);
                zeroed_offsets.insert(*sensor, r_zero);
                break;
            }

            let r_zero = apply_offset(&rotations(&zero)[*rotation_idx], offset);

            visited.insert(relative_to);
            for (relative_to, (offset, rotation_idx)) in offsets.get(relative_to).unwrap() {
                if !visited.contains(relative_to) {
                    q.push_back((relative_to, offset, rotation_idx, r_zero));
                }
            }
        }
    }


    let mut largest = 0;
    for (x1, y1, z1) in zeroed_offsets.values() {
        for (x2, y2, z2) in zeroed_offsets.values() {
            let manhattan = (x1 - x2).abs() + (y1 - y2).abs() + (z1 - z2).abs();
            if manhattan > largest {
                largest = manhattan;
            }
        }
    }
    println!("{}", largest);
}

fn get_offset_and_rotate(absolute: &HashSet<V3>, relative: &HashSet<V3>) -> Option<(V3, usize)> {
    for i in 0..rotations(&(0,0,0)).len() {
        let relative: HashSet<V3> = relative.iter().map(|a| rotations(a)[i]).collect();

        if let Some(offset) = get_offset(absolute, &relative) {
            return Some((offset, i))
        }
    }

    None
}

fn get_offset(absolute: &HashSet<V3>, relative: &HashSet<V3>) -> Option<V3> {
    for b in relative {
        for a in absolute {
            let test = (a.0 - b.0, a.1 - b.1, a.2 - b.2);

            let common = relative.into_iter()
                .filter(|b| absolute.contains(&apply_offset(b, &test)))
                .count();

            if common >= 12 {
                return Some(test);
            }
        }
    }

    None
}

fn apply_offset((x1, y1, z1): &V3, (x2, y2, z2): &V3) -> V3 {
    (x1+x2, y1+y2, z1+z2)
}

fn rotations(o: &V3) -> Vec<V3> {
    let (x, y, z) = *o;
    let perms = [x, y, z].into_iter()
        .permutations(3)
        .collect::<Vec<Vec<_>>>();
    let perms = perms.into_iter().map(|p| (p[0], p[1], p[2])).collect::<Vec<_>>();

    let mut res = Vec::<V3>::new();
    res.extend_from_slice(perms.as_slice());

    for (a, b, c) in perms {
        res.push((a, b, c * -1)); // 0 0 1
        res.push((a, b * -1, c)); // 0 1 0
        res.push((a, b * -1, c * -1)); // 0 1 1
        res.push((a * -1, b, c)); // 1 0 0
        res.push((a * -1, b, c * -1)); // 1 0 1
        res.push((a * -1, b * -1, c)); // 1 1 0
        res.push((a * -1, b * -1, c * -1)); // 1 1 1
    }

    res
}
