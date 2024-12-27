#[derive(Debug)]

enum Tree {
    Node (i32, Box<Tree>, Box<Tree>),
    Empty,
}

impl Tree {
    // Define an empty tree
    fn new() -> Tree {
        Tree::Empty
    }
    // Insert an element into a tree (BST)
    fn insert(value: i32, bst: Tree) -> Tree {
        match bst {
            Tree::Empty => Tree::Node(value, Box::new(Tree::Empty), Box::new(Tree::Empty)),
            Tree::Node(v, left, right) => {
                if value < v {
                    Tree::Node(v, Box::new(Tree::insert(value, *left)), right)    // Insert node left subtree
                } else {
                    Tree::Node(v, left, Box::new(Tree::insert(value, *right)))    // Insert node right subtree
                }
            }
        }

    }
    // Build a BST from vector
    fn from_vec(vec: &Vec<i32>) -> Tree {
        let mut tree = Tree::new();
        for &value in vec {
            tree = Tree::insert(value, tree)
        }
        tree
    }
    // Check if an element is in the BST
    fn elem(value: i32, bst: Tree) -> bool {
        match bst {
            Tree::Empty => false,
            Tree::Node(v, left, right) => {
                if v == value {
                    true
                } else {
                    if value < v {
                        Tree::elem(value, *left)
                    } else {
                        Tree::elem(value, *right)
                    }
                }
            }
        }
    }
}

fn main() {
    let v = vec![10,5,15,3,7,12,18];
    let bst = Tree::from_vec(&v);
    let find = 7;
    println!("Input vector: {:?}", v);
    println!("BST:");
    println!("{:?}", bst);
    println!("{find} is in the BST: {}", Tree::elem(find,bst));
}