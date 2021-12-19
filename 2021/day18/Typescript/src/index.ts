import { readFile } from 'fs/promises';

class Node {
	left!: Tree
	right!: Tree
	parent?: Node
	constructor(left: Tree, right: Tree) {
		this.addLeft(left);
		this.addRight(right);
	}

	addLeft(n: Tree) {
		n.parent = this;
		this.left = n;
	}

	addRight(n: Tree) {
		n.parent = this;
		this.right = n;
	}

	toString(): string {
		return `[${this.left.toString()}, ${this.right.toString()}]`;
	}
}

class Leaf {
	value: number;
	parent?: Node;
	constructor(value: number) {
		if (isNaN(value)) {
			throw new Error(`NaN`);
		}
		this.value = value;
	}

	toString(): string {
		return this.value.toString();
	}
}

type Tree = Node | Leaf;

function replace(node: Tree, newVal: Tree) {
	let p = node.parent;
	newVal.parent = p;
	if (!p) throw new Error(`Can't replace the root node!`);

	if (node === p.left) {
		p.left = newVal;
	} else {
		p.right = newVal;
	}
}

function toTree(v: number | Array<any>): Tree {
	if (typeof v == 'number') {
		return new Leaf(v);
	} else {
		return new Node(toTree(v[0]), toTree(v[1]));
	}
}

function getRoot(s: Tree): Node {
	if (!s.parent) return s as Node;
	return getRoot(s.parent);
}

function step(s: Tree): boolean {
	if (s instanceof Leaf) return false;
	if (tryExplode(s, 0)) return true;
	if (trySplit(s)) return true;

	return false;
}

function tryExplode(node: Tree, depth: number): boolean {
	if (node instanceof Leaf) return false;

	if (depth >= 4) {
		if (!(node.left instanceof Leaf) || !(node.right instanceof Leaf)) {
			return tryExplode(node.left, depth + 1) || tryExplode(node.right, depth + 1);
		}

		// KABOOM
		const l = node.left.value;
		const r = node.right.value;

		// Find first left-leaf
		let leftN = (() => {
			let n = node;
			let p = node.parent;
			while (p?.left === n) {
				n = p;
				p = n.parent;
			}
			return p;
		})();
		if (leftN) {
			if (leftN.left instanceof Node) {
				let n: Tree = leftN.left;
				while (n.right instanceof Node) { n = n.right; }
				(n.right as Leaf).value += l;
			} else {
				leftN.left.value += l;
			}
		}

		// Same for right
		let rightN = (() => {
			let n = node;
			let p = node.parent;
			while (p?.right === n) {
				n = p;
				p = n.parent;
			}
			return p;
		})();
		if (rightN) {
			if (rightN.right instanceof Node) {
				let n: Tree = rightN.right;
				while (n.left instanceof Node) { n = n.left; }
				(n.left as Leaf).value += r;
			} else {
				rightN.right.value += r;
			}
		}

		replace(node, new Leaf(0));

		return true;
	}

	return tryExplode(node.left, depth + 1) || tryExplode(node.right, depth + 1);
}

function trySplit(node: Tree): boolean {
	if (node instanceof Node) {
		return trySplit(node.left) || trySplit(node.right);
	}

	if (node.value >= 10) {
		replace(node, new Node(new Leaf(Math.floor(node.value / 2)), new Leaf(Math.ceil(node.value / 2))));
		return true;
	}

	return false;
}

function add(a: Node, b: Node): Node {
	return new Node(a, b);
}

(async () => {
	const raw = await readFile('inputT.txt').then(b => b.toString());
	const snailfish = raw.split('\n').map(l => toTree(JSON.parse(l)));

	let added = snailfish.slice(1).reduce((acc, s) => {
		const added = add(acc as Node, s as Node);
		// step(added);
		return added;
	}, snailfish[0]);

	console.log(added.toString());
	while (step(added)) {
		console.log(added.toString());
	}
	console.log('--------------');
})().catch(e => {
	console.error(e);
	process.exit(1);
});
