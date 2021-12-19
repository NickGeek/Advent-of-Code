use std::rc::Rc;
use std::sync::RwLock;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum Smallfish {
    Leaf(i64),
    Node(Box<(Smallfish, Smallfish)>)
}

#[derive(Debug, Clone)]
enum SmallfishValue {
    Leaf(i64),
    Node(Node, Node)
}

#[derive(Debug, Clone)]
struct SmallfishNode {
    v: Box<SmallfishValue>,
    parent: Option<Node>
}

type Node = Rc<RwLock<SmallfishNode>>;

fn main() {
    let raw = std::fs::read_to_string("inputT.txt").unwrap();
    let smallfish = raw.lines()
        .map(|smallfish| serde_json::from_str::<(Smallfish, Smallfish)>(smallfish).unwrap())
        .map(|root| build_tree(Smallfish::Node(Box::new(root)), None))
        .collect::<Vec<_>>();
}

fn build_tree(s: Smallfish, parent: Option<Rc<RwLock<SmallfishNode>>>) -> Rc<RwLock<SmallfishNode>> {
    let n = Rc::new(RwLock::new(SmallfishNode {
        v: Box::new(SmallfishValue::Leaf(0)),
        parent
    }));

    let v = match s {
        Smallfish::Leaf(n) => SmallfishValue::Leaf(n),
        Smallfish::Node(nb) => {
            let (l, r) = *nb;
            SmallfishValue::Node(build_tree(l, Some(n.clone())), build_tree(r, Some(n.clone())))
        }
    };

    {
        let mut n = n.write().unwrap();
        n.v = Box::new(v);
    }
    n
}

fn step(n: Node) {

}

fn maybe_explode(n: Node, c: u8) -> Node {
    let n = n.read().unwrap();
    match &*n.v {
        SmallfishValue::Leaf(v) => return n
        SmallfishValue::Node(l, r) => {

        }
    }
}

// fn handle_deeply_nested(p: &Pair, c: u8) -> (Pair, Option<(i64, i64)>) {
//     let (s_l, s_r) = p;
//     if c == 3 {
//         match s_l {
//             Smallfish::Nested(n) => {
//                 let (n_l, n_r) = *n.clone();
//                 let mut n_l = match n_l { Smallfish::Regular(n) => *n, _ => unreachable!() };
//                 let mut n_r = match n_r { Smallfish::Regular(n) => *n, _ => unreachable!() };
//
//                 let s_r = match s_r {
//                     Smallfish::Regular(s_r_n) => {
//                         let n_v = n_r;
//                         n_r = 0;
//                         Smallfish::Regular(*s_r_n + n_v)
//                     },
//                     _ => s_r.clone()
//                 };
//
//                 ((Smallfish::Regular(0), s_r), Some((n_l, n_r)));
//             }
//             _ => s_l
//         }
//         match s_r {
//             Smallfish::Nested(n) => {
//                 let (n_l, n_r) = *n.clone();
//                 let mut n_l = match n_l { Smallfish::Regular(n) => *n, _ => unreachable!() };
//                 let mut n_r = match n_r { Smallfish::Regular(n) => *n, _ => unreachable!() };
//
//                 let s_l = match s_l {
//                     Smallfish::Regular(s_l_n) => {
//                         let n_v = n_l;
//                         n_l = 0;
//                         Smallfish::Regular(*s_l_n + n_v)
//                     },
//                     _ => s_l.clone()
//                 };
//
//                 ((s_l, Smallfish::Regular(0)), Some((n_l, n_r)));
//             }
//             _ => {}
//         }
//     }
//
//     let s_r: Smallfish = match s_r {
//         Smallfish::Nested(p2) => {
//             let (p, exp) = handle_deeply_nested(&*p2, c + 1);
//             if let Some((exp_l, _)) = exp {
//                 let (n_l, _) = p.clone();
//
//                 let s_l = match s_l {
//                     Smallfish::Regular(s_l_n) => {
//                         Smallfish::Regular(*s_l_n + n_l)
//                     },
//                     _ => s_l.clone()
//                 };
//                 return Smallfish::Nested(s_l, )
//             } else { Smallfish::Nested(Box::new(p)) }
//         },
//         Smallfish::Regular(l) =>
//     };
// }
//
// fn is_deeply_nested(s: &Smallfish, c: u8) -> bool {
//     if c == 4 { return true; }
//     match s_l {
//         Smallfish::Nested(p2) => handle_deeply_nested(&*p2, c + 1),
//         Smallfish::Regular(l) =>
//     }
// }
