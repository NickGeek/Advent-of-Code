const INPUT_T: &str = "";
const INPUT: &str = "";

fn main() {
    println!("{:?}", p1(INPUT_T));
    println!("\n---------------\nfinal answer:\n{:?}", p1(INPUT));
}

fn p1(input: &str) {
    let mut a = input.split("\n\n")
        .map(|elf| elf.lines().map(|cal| cal.parse::<u64>().unwrap()).sum::<u64>())
        .collect::<Vec<_>>();
    a.sort();

    println!("{:?}", p1(INPUT_T));
    println!("\n---------------\nfinal answer:\n{:?}", p1(INPUT));
}