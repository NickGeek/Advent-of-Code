use std::collections::HashSet;

type Pos = (usize, usize);

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let graph = raw.lines()
        .map(|l|
            l.chars()
                .map(|c| c.to_string().parse::<usize>().unwrap())
                .collect()
        )
        .collect::<Vec<Vec<usize>>>();

    part1(graph.clone());
    part2(graph);
}

fn part1(mut graph: Vec<Vec<usize>>) {
    let mut flashes = 0;
    for _ in 0..195 {
        flashes += step(graph.as_mut_slice());
    }
    println!("{:?}", graph);
    println!("{:?}", flashes);
}

fn part2(mut graph: Vec<Vec<usize>>) {
    let mut i = 0;
    let goal = graph.len() * graph[0].len();
    loop {
        i += 1;
        if step(graph.as_mut_slice()) == goal {
            println!("Part 2: {}", i);
            break;
        }
    }
}

fn step(graph: &mut [Vec<usize>]) -> usize {
    graph.iter_mut()
        .flat_map(|row| row.iter_mut())
        .for_each(|el| *el += 1);

    let mut flashed = HashSet::<Pos>::new();
    for y in 0..graph.len() {
        for x in 0..graph[y].len() {
            try_flash(&mut flashed, graph, (x, y));
        }
    }

    flashed.len()
}

fn try_flash(flashed: &mut HashSet<Pos>, graph: &mut [Vec<usize>], node: Pos) {
    if flashed.contains(&node) || get(node, graph) <= 9 { return; }
    let (x, y) = node;

    graph[y][x] = 0;
    flashed.insert((x, y));
    let neigh = neighbours((x, y), graph);
    for n in neigh {
        if flashed.contains(&n) { continue; }

        let (nx, ny) = n;
        graph[ny][nx] += 1;

        try_flash(flashed, graph, n);
    }
}

fn get((x, y): Pos, graph: &[Vec<usize>]) -> usize {
    graph[y][x]
}

fn neighbours((x, y): Pos, graph: &[Vec<usize>]) -> HashSet<Pos> {
    let mut n = HashSet::new();

    for y in (y as isize)-1..=(y as isize)+1 {
        for x in (x as isize)-1..=(x as isize)+1 {
            let x = x as usize;
            let y = y as usize;
            if let Some(_) = graph.get(y).and_then(|v| v.get(x)) {
                n.insert((x, y));
            }
        }
    }

    n
}
