use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::rc::Rc;
use itertools::Itertools;

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

#[derive(Clone, Debug)]
struct Fs {
    name: String,
    children: Vec<Rc<RefCell<Fs>>>,
    files: Vec<RefCell<File>>,
    parent: Option<Rc<RefCell<Fs>>>
}
impl Fs {
    fn new(name: String, parent: Option<Rc<RefCell<Fs>>>) -> Self {
        Fs { name, children: vec![], files: Vec::new(), parent }
    }
    fn size_kinda(&self) -> usize {
        let size = self.size();
        let rest = self.children.iter().map(|c|c.as_ref().borrow().size_kinda()).sum::<usize>();
        if size > 100000 { return rest; }
        // println!("{}", size);
        size + rest
    }
    fn files_size(&self) -> usize {
        self.files.iter()
            .dedup_by(|a,b| {
                let a = &**a;
                let b = &**b;
                a.borrow().name == b.borrow().name
            })
            .map(|f| f.borrow().size).sum::<usize>()
    }
    fn size(&self) -> usize {
        self.files_size() + self.children.iter()
            .map(|fs| (&**fs).borrow().size())
            .sum::<usize>()
    }
    fn children_sizes(&self, required: usize, acc: &mut Vec<usize>) {
        let size = self.size();
        if size >= required { acc.push(self.size()) };
        self.children.iter()
            .for_each(|c| c.as_ref().borrow().children_sizes(required, acc));
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct File {
    name: String,
    size: usize
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum Cmd {
    Empty,
    Cd,
    Ls
}

fn p1(input: &str) {
    let mut fs = Rc::new(RefCell::new(Fs::new(
        "/".to_string(),
        None
    )));
    fs.as_ref().borrow_mut().parent = Some(fs.clone());
    let root = fs.clone();
    let mut cmd = Cmd::Empty;
    for cmd_or_out in input.lines() {
        if cmd_or_out.starts_with('$') {
            if cmd_or_out == "$ ls" { cmd = Cmd::Ls }
            if cmd_or_out.starts_with("$ cd ") {
                let arg = cmd_or_out.split("$ cd ").nth(1).unwrap();
                match arg {
                    ".." => {
                        let _fs = fs.as_ref().borrow().parent.clone().unwrap();
                        fs = _fs;
                    },
                    "/" => fs = root.clone(),
                    path => {
                        let eelse = Rc::new(RefCell::new(Fs::new(path.to_string(), Some(fs.clone()))));
                        let _fs = fs.as_ref().borrow().children.iter()
                            .find(|c| c.as_ref().borrow().name == path)
                            .unwrap_or(&eelse)
                            .clone();
                        fs = _fs;
                    }
                }
            }
        } else {
            // output
            match cmd {
                Cmd::Empty => unreachable!("{:?}", cmd_or_out),
                Cmd::Cd => unreachable!(),
                Cmd::Ls => {
                    if cmd_or_out.starts_with("dir") {
                        let name = cmd_or_out.split_once("dir ").unwrap().1.to_string();
                        fs.as_ref().borrow_mut().children.push(Rc::new(RefCell::new(Fs::new(name, Some(fs.clone())))));
                        continue;
                    }

                    let (size, name) = cmd_or_out.split_once(' ').unwrap();
                    // let mut fs = fs.borrow_mut();
                    fs.as_ref().borrow_mut().files.push(RefCell::new(File { name: name.to_string(), size: size.parse::<usize>().unwrap() }));
                }
            }
        }
    }
    let res: usize = root.as_ref().borrow().size_kinda();

    println!("{:?}", res);
}

fn p2(input: &str) {
    const DISK_SPACE: usize = 70000000;
    const NEEDED_FREE: usize = 30000000;

    // Compute fs graph
    let mut fs = Rc::new(RefCell::new(Fs::new(
        "/".to_string(),
        None
    )));
    fs.as_ref().borrow_mut().parent = Some(fs.clone());
    let root = fs.clone();
    let mut cmd = Cmd::Empty;
    for cmd_or_out in input.lines() {
        if cmd_or_out.starts_with('$') {
            if cmd_or_out == "$ ls" { cmd = Cmd::Ls }
            if cmd_or_out.starts_with("$ cd ") {
                let arg = cmd_or_out.split("$ cd ").nth(1).unwrap();
                match arg {
                    ".." => {
                        let _fs = fs.as_ref().borrow().parent.clone().unwrap();
                        fs = _fs;
                    },
                    "/" => fs = root.clone(),
                    path => {
                        let eelse = Rc::new(RefCell::new(Fs::new(path.to_string(), Some(fs.clone()))));
                        let _fs = fs.as_ref().borrow().children.iter()
                            .find(|c| c.as_ref().borrow().name == path)
                            .unwrap_or(&eelse)
                            .clone();
                        fs = _fs;
                    }
                }
            }
        } else {
            // output
            match cmd {
                Cmd::Empty => unreachable!("{:?}", cmd_or_out),
                Cmd::Cd => unreachable!(),
                Cmd::Ls => {
                    if cmd_or_out.starts_with("dir") {
                        let name = cmd_or_out.split_once("dir ").unwrap().1.to_string();
                        fs.as_ref().borrow_mut().children.push(Rc::new(RefCell::new(Fs::new(name, Some(fs.clone())))));
                        continue;
                    }

                    let (size, name) = cmd_or_out.split_once(' ').unwrap();
                    // let mut fs = fs.borrow_mut();
                    fs.as_ref().borrow_mut().files.push(RefCell::new(File { name: name.to_string(), size: size.parse::<usize>().unwrap() }));
                }
            }
        }
    }

    let left = DISK_SPACE - root.as_ref().borrow().size();
    let required = NEEDED_FREE - left;

    let root = root.as_ref().borrow(); // technically this won't include / but I think it's fine
    // let mut r = root.children.iter()
    //     .filter(|c| c.as_ref().borrow().size() >= required)
    //     .map(|c| c.as_ref().borrow().size())
    //     .collect::<Vec<_>>();
    let mut r = Vec::new();
    root.children_sizes(required, &mut r);
    r.sort();

    let res: usize = r[0];
    println!("{:?}", res);
}
