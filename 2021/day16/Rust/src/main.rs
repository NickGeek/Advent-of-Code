fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    part1(&*raw);
    part2(&*raw);
}

#[derive(Debug, Clone)]
struct Packet {
    version: usize,
    type_id: usize,
    lit_val: String,
    sub_packets: Vec<Packet>
}
impl Default for Packet {
    fn default() -> Self {
        Self {
            version: 0,
            type_id: 0,
            lit_val: "".to_string(),
            sub_packets: vec![]
        }
    }
}

enum LengthType {
    Short, // 11
    Long // 15
}

fn part1(raw: &str) {
    let hex = raw;
    let binary_str = hex.chars().map(|n| to_binary(n)).collect::<String>();

    let (p, _) = parse_packet(&*binary_str).unwrap();

    println!("{:?}", p);
    println!("{:?}", count_versions(&p));
}

fn parse_packet(binary_str: &str) -> Result<(Packet, usize), usize> {
    let mut p = Packet::default();
    if binary_str.len() <= 6 {
        return Err(binary_str.len());
    }

    let mut cur: usize = 0;
    p.version = to_dec(get_range(binary_str, cur, cur+3));
    cur += 3;
    p.type_id = to_dec(get_range(binary_str, cur, cur+3));
    cur += 3;

    if p.type_id == 4 {
        // literal
        while get_c(cur, binary_str) == '1' {
            cur += 1;
            p.lit_val.push_str(get_range(binary_str, cur, cur+4));
            cur += 4;
        }
        cur += 1;
        p.lit_val.push_str(get_range(binary_str, cur, cur+4));
        cur += 4;
    } else {
        // Op
        let len = if binary_str.as_bytes()[cur] == b'1' { 11 } else { 15 };
        cur += 1;

        let sub_len = to_dec(get_range(binary_str, cur, cur+len));
        cur += len;

        if len == 11 {
            for _ in 0..sub_len {
                match parse_packet(get_range(binary_str, cur, binary_str.len())) {
                    Ok((sub, consumed)) => {
                        p.sub_packets.push(sub);
                        cur += consumed;
                    },
                    Err(consumed) => {
                        cur += consumed;
                    }
                }
            }
        } else {
            let mut max = cur + sub_len;

            while cur < max {
                match parse_packet(get_range(binary_str, cur, max)) {
                    Ok((sub, consumed)) => {
                        p.sub_packets.push(sub);
                        cur += consumed;
                    },
                    Err(consumed) => {
                        cur += consumed
                    }
                }
            }
        }
    }

    Ok((p, cur))
}

fn get_c(cur: usize, bin_str: &str) -> char {
    bin_str.as_bytes()[cur] as char
}

fn get_range(bin_str: &str, from: usize, to: usize) -> &str {
    &bin_str[from..to]
}

fn count_versions(p: &Packet) -> usize {
    let children: usize = p.sub_packets.iter().map(|p| count_versions(p)).sum();
    p.version + children
}

fn part2(raw: &str) {
    let hex = raw;
    let binary_str = hex.chars().map(|n| to_binary(n)).collect::<String>();

    let (p, _) = parse_packet(&*binary_str).unwrap();
    println!("Part 2: {}", exec(&p));
}

fn exec(p: &Packet) -> usize {
    match p.type_id {
        0 => p.sub_packets.iter().map(|p| exec(p)).sum(),
        1 => p.sub_packets.iter().map(|p| exec(p)).product(),
        2 => p.sub_packets.iter().map(|p| exec(p)).min().unwrap(),
        3 => p.sub_packets.iter().map(|p| exec(p)).max().unwrap(),
        4 => to_dec(&*p.lit_val),
        5 => {
            if exec(&p.sub_packets[0]) > exec(&p.sub_packets[1]) { 1 } else { 0 }
        },
        6 => {
            if exec(&p.sub_packets[0]) < exec(&p.sub_packets[1]) { 1 } else { 0 }
        },
        7 => {
            if exec(&p.sub_packets[0]) == exec(&p.sub_packets[1]) { 1 } else { 0 }
        },
        _ => unreachable!()
    }
}

fn to_dec(bin: &str) -> usize {
    usize::from_str_radix(bin, 2).unwrap()
}

fn to_binary(c: char) -> &'static str {
    match c {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        _ => panic!("Bad hex"),
    }
}
