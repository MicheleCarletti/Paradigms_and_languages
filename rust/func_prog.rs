#[derive (PartialEq,Debug)]

struct Shoe {
    size: u32,
    style: String,
}

fn main() {
    let mut list = vec![1,2,3];
    println!("Before defining closure: {list:?}");

    let mut borrows_mutability = || list.push(5);

    //println!("Before calling closure: {list:?}"); // Compilation fails cause borrows_mutability captures a mutable reference to list
    borrows_mutability();   // Here the borrow ends, so it's safe to call an immutable borrow again
    println!("After calling closure: {list:?}");

    /*Example with iterators*/
    iter();

    /* Example of functional programming functions*/
    funct();


}

fn iter() {
    let shoes = vec![
        Shoe {size: 10, style: String::from("sneaker")},
        Shoe {size:13, style: String::from("sandal")},
        Shoe {size: 10, style: String::from("boot")},
    ];

    let in_my_size = shoes_in_size(shoes, 10);
    println!("{in_my_size:?}");

    assert_eq!(
        in_my_size,
        vec![
            Shoe {
                size: 10,
                style: String::from("sneaker"),
            },
            Shoe {
                size: 10,
                style: String::from("boot"),
            },
        ]
    );


}
/* Takes ownership of a vector of shoes and returns a vector containing only shoes of the specified size */
fn shoes_in_size (shoes: Vec<Shoe>, size: u32) -> Vec<Shoe> {
    shoes.into_iter().filter(|s| s.size == size).collect()
}

fn funct() {
    let vector = (1..)  // Infinite list
        .filter(|x| x % 2 != 0) // Filters odd numbers
        .take(5)    // Takes first five
        .map(|x| x * x) // Square each value
        .collect::<Vec<usize>>();   // Return as a vector of usize
    
    println!("{vector:?}");
}