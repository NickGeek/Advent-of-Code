use std::fs;

fn main() {
    let raw = fs::read_to_string("input.txt").unwrap();

    let mut counts: [isize; 9] = [0; 9];
    raw.split(',').map(|v| v.parse::<usize>().unwrap()).for_each(|v| {
        counts[v] += 1;
    });

    let days = 256;
    for _ in 0..days {
        let deads = counts[0];
        for s in 1..=8 {
            let c = counts[s];
            counts[s] -= c;
            counts[s - 1] += c;
        }
        counts[0] -= deads;
        counts[6] += deads;
        counts[8] += deads;
    }

    let mut sum = 0;
    for c in counts {
        sum += c;
    }

    println!("{:?}", counts);
    println!("{}", sum);
}
