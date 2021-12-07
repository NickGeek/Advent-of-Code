use std::collections::HashMap;
use std::fs;

fn main() {
    let raw = fs::read_to_string("input.txt").unwrap();
    let mut grid = HashMap::<isize, isize>::new();
    raw.split(',').map(|v| v.parse::<isize>().unwrap()).for_each(|crab| {
        let c_val = if let Some(c) = grid.get(&crab) { *c } else { 0 };
        grid.insert(crab, c_val + 1);
    });

    let max = *grid.keys().max().unwrap();

    let mut cheapest = -1;
    let mut cheapest_cost = -1;
    for i in 0..=max {
        // How much does it cost to move to k?
        let mut cost = 0;
        for j in 0..=max {
            let diff = (i - j).abs();
            let mut local_cost = 0;
            for k in 0..=diff {
                local_cost += k;
            }

            let n_crabs = if let Some(n) = grid.get(&j) { *n } else { 0 };
            cost += local_cost * n_crabs;
        }
        if cost < cheapest_cost || cheapest_cost == -1 {
            cheapest = i;
            cheapest_cost = cost;
        }
    }

    println!("{}", cheapest);
    println!("{}", cheapest_cost);
}
