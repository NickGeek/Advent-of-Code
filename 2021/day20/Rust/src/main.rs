use std::collections::HashMap;
use rayon::prelude::*;

type Pos = (i64, i64);

#[derive(Debug, Copy, Clone)]
enum Pixel {
    Light,
    Dark
}
impl From<char> for Pixel {
    fn from(c: char) -> Self {
        if c == '#' { Pixel::Light } else { Pixel::Dark }
    }
}
impl Default for Pixel {
    fn default() -> Self {
        Pixel::Dark
    }
}

const GRID_SIZE: Pos = (-3, 103);

fn main() {
    let raw = std::fs::read_to_string("input.txt").unwrap();
    let (enhancement, image_raw) = raw.split_once("\n\n").unwrap();

    let algo: Vec<Pixel> = enhancement.chars()
        .map(|char| Pixel::from(char))
        .collect();

    let image: HashMap<Pos, Pixel> = image_raw.lines().enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate()
                .map(move |(x, char)| ((x as i64, y as i64), Pixel::from(char)))
        })
        .collect();


    let mut proc = image;
    print_grid(&proc, GRID_SIZE, GRID_SIZE);
    for i in 0..2 {
        proc = step(&proc, &algo, i);
        print_grid(&proc, GRID_SIZE, GRID_SIZE);
    }

    let lit = proc.values()
        .filter(|p| {
            match p {
                Pixel::Light => true,
                Pixel::Dark => false
            }
        })
        .count();

    // println!("{:?}", get_pixel(&image, &algo, (2, 2)));
    println!("{:?}", lit);
}

fn step(image: &HashMap<Pos, Pixel>, algo: &[Pixel], n: usize) -> HashMap<Pos, Pixel> {
    let max_x = image.keys().map(|(x, y)| *x).max().unwrap();
    let max_y = image.keys().map(|(x, y)| *y).max().unwrap();
    let min_x = image.keys().map(|(x, y)| *x).min().unwrap();
    let min_y = image.keys().map(|(x, y)| *y).min().unwrap();

    ((min_y - 1)..=(max_y + 1)).into_par_iter()
        .flat_map(|y| {
            ((min_x - 1)..=(max_x + 1)).into_par_iter()
                .map(move |x| ((x, y), get_pixel(&image, algo, (x, y), n)))
        })
        .collect::<HashMap<Pos, Pixel>>()
}

fn print_grid(image: &HashMap<Pos, Pixel>, (x1, x2): Pos, (y1, y2): Pos) {
    let default = Pixel::Dark;
    for y in y1..y2 {
        for x in x1..x2 {
            match image.get(&(x, y)).unwrap_or(&default) {
                Pixel::Light => print!("#"),
                Pixel::Dark => print!(".")
            }
        }
        print!("\n");
    }
    print!("\n");
}

fn get_pixel(image: &HashMap<Pos, Pixel>, algo: &[Pixel], pos: Pos, n: usize) -> Pixel {
    let mut number = String::with_capacity(9);

    let default = if n % 2 == 0 { Pixel::Dark } else { Pixel::Light };

    let (x, y) = pos;
    for y in y-1..=y+1 {
        for x in x-1..=x+1 {
            match image.get(&(x, y)).unwrap_or(&default) {
                Pixel::Light => { number.push('1'); }
                Pixel::Dark => { number.push('0'); }
            }
        }
    }

    let idx = to_dec(&*number);
    algo[idx]
}

fn to_dec(bin: &str) -> usize {
    usize::from_str_radix(bin, 2).unwrap()
}
