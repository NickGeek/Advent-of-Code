fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let map = raw.lines().map(|line| {
        line.chars().collect::<Vec<char>>()
    }).collect::<Vec<_>>();

    let mut prev = map;
    let mut count = 1;
    loop {
        // print_map(&prev);
        // println!("\n\n\n");
        let next = step(&prev);
        if prev == next { break; }
        prev = next;

        count += 1;
    }

    println!("{}", count);
}

fn step(map: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_map = map.clone();

    for y in 0..map.len() {
        for x in 0..map[y].len() {
            let spot = map[y][x];
            if spot != '>' { continue; }

            if let Some(next) = map[y].get(x + 1) {
                if next == &'.' {
                    new_map[y][x] = '.';
                    new_map[y][x+1] = spot;
                }
            } else {
                let next = map[y][0];
                if next == '.' {
                    new_map[y][x] = '.';
                    new_map[y][0] = spot;
                }
            }
        }
    }

    let map = &new_map.clone();
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            let spot = map[y][x];
            if spot != 'v' { continue; }

            if let Some(next) = map.get(y + 1).map(|row| row[x]) {
                if next == '.' {
                    new_map[y][x] = '.';
                    new_map[y+1][x] = spot;
                }
            } else {
                let next = map[0][x];
                if next == '.' {
                    new_map[y][x] = '.';
                    new_map[0][x] = spot;
                }
            }
        }
    }

    new_map
}

fn print_map(map: &Vec<Vec<char>>) {
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            print!("{}", map[y][x]);
        }

        print!("\n");
    }
}
