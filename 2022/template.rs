fn main() {
    let input_t = &*std::fs::read_to_string("input_t.txt").unwrap();
    let input = &*std::fs::read_to_string("input.txt").unwrap();
    p1(input_t);
    println!("\n---------------\nfinal answer:");
    p1(input);
    println!("\n---------------\np2:");
    p2(input_t);
    println!("\n---------------\nfinal answer:");
    p2(input);
}

fn p1(input: &str) {
    let mut a = input.lines()
        .map(|round| round.split(' ').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let res: u64 = 0;

    println!("{:?}", res);
}

fn p2(input: &str) {
    let mut a = input.lines()
        .map(|round| round.split(' ').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let res: u64 = 0;

    println!("{:?}", res);
}
