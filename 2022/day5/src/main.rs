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

const INPUT_T: &str = "";
const INPUT: &str = "";
