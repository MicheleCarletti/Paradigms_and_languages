fn main() {
    let s1 = String::from("hello");
    let s2 = s1;    // After this point s1 is no longer valid due to ownership rules

    println!("{s2}");

    /* Use the clone() method to duplicate also the heap of a variable*/
    let s3 = String::from("world");
    let s4 = s3.clone();    // Clone the variable, duplivcates also the heap of s3
    println!("{s3} {s4}");

    /*Integers are static, they're completely saved in the stack*/
    let x = 5;
    let y = x;
    println!("x = {x}, y = {y}");   // x and y are both stored in the stack

    let s = String::from("I'm a string");   // s comes into the scope

    takes_ownership(s); // s value moves into the function...
                        // ... and so is no longer valid here 

    let z = 5;  // z comes into the scope

    makes_copy(z);  // z would move into the function,
                    // but i32 is Copy, so it's okay to still
                    // use z afterwards

    /* To avoid problems with ownership, while calling functions we can use reference*/
    let s5 = String::from("Good morning");

    let len = calculate_length(&s5);    // Passing the reference of s5
    println!("The length of '{s5}' is {len}");

    /* We can modify an element through its reference (the variable and the reference must be mutables!!)*/
    let mut s6 = String::from("Hello");
    change(&mut s6);
    println!("{s6}");
}

fn takes_ownership(some_string: String) {   // some_string comes into scope
    println!("{some_string}");
}   // Here some_string goes out of scope, and 'drop' is called. The backing memory is freed.

fn makes_copy(some_integer: i32) {  // some_integer comes into scope
    println!("{some_integer}");
}   // Here some_integer goues out of scope, and 'drop' is called.  

fn calculate_length(s: &String) -> usize {  // s is a reference of String
    s.len()
}

fn change(s: &mut String) { // It takes a reference of a mutable String and chages it directly w/o returning anything
    s.push_str(", world!");
}