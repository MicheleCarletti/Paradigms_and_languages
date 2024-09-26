use crate::garden::vegetables::{Vegetable, color};  // Call the Vegetable structure and function color() from module vegetables

pub mod garden; // Define the public module garden, implemented in src/garden.rs

fn main() {
    let v1 = Vegetable::Asparagus;
    let v2 = Vegetable::Tomato;

    println!("Hi, I'm {v1:?}");
    println!("My color is: {}", color(&v1));

    println!("\nHi, I'm {v2:?}");
    println!("My color is: {}", color(&v2));
}
