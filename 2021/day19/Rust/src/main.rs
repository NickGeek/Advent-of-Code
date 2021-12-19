use std::collections::{HashMap, HashSet};
use rayon::prelude::*;

type V3 = (i64, i64, i64);

fn main() {
    let raw = std::fs::read_to_string("./inputT.txt").unwrap();
    let r = raw.split("\n\n")
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

    // println!("{:?}", r);

    println!("{:#?}", get_offset_and_rotate(&r[0], &r[1]));
}

fn get_offset_and_rotate(absolute: &HashSet<V3>, relative: &HashSet<V3>) -> V3 {
    let f = rotations(&(-618,-824,-621)).iter().position(|a| a == &(-618+68,-824-1246,-621-43)).unwrap();
    let foo: HashSet<V3> = relative.iter().map(|a| rotations(a)[f]).collect();
    println!("{:?}", foo);

    let a = get_offset(absolute, &foo);

    for i in 0..24 {
        let relative: HashSet<V3> = relative.iter().map(|a| rotations(a)[i]).collect();

        if let Some(offset) = get_offset(absolute, &relative) {
            return offset
        }
    }

    panic!("No offset found");
}

fn get_offset(absolute: &HashSet<V3>, relative: &HashSet<V3>) -> Option<V3> {
    for b in relative {
        for a in absolute {
            // let test = (a.0 - b.0, a.1 - b.1, a.2 - b.2);
            let test = (68,-1246,43);

            let common = relative.into_iter()
                .filter(|b| {
                    // if absolute.contains(&apply_offset(b, &test)) {
                    //     println!("found {:?} using {:?}", &apply_offset(b, &test), test);
                    // }

                    absolute.contains(&apply_offset(b, &test))
                })
                .count();

            println!("{}", common);

            if common >= 12 {
                return Some(test);
            }
        }
    }

    None
}

fn apply_offset(v3: &V3, offset: &V3) -> V3 {
    (v3.0 + offset.0, v3.1 + offset.1, v3.2 + offset.2)
}

fn rotations(o: &V3) -> [V3; 24] {
    let (x, y, z) = *o;
    let perms = [
        (x, y, z),
        (x, z, y),
        (y, x, z),
        (y, z, x),
        (z, y, x),
        (z, x, y)
    ];

    let mut res = Vec::<V3>::with_capacity(24);
    res.extend_from_slice(&perms);

    for (a, b, c) in perms {
        res.push((a * -1, b, c));
        res.push((a * -1, b * -1, c));
        res.push((a * -1, b * -1, c * -1));
    }

    res.try_into().unwrap()
}
