const MONTHS: u8 = 12;  // This is a global constant of type u8
fn main(){
    println!("There are {} months in a year", MONTHS);

    let x = 1;  // This is immutable variable
    println!("The value of the immutable variable is  {}", x);

    let mut y = 5;  // This is mutable variable
    while y > 0 {
        println!("Countdown! {}", y);
        y = y - 1;
    }

    let a = [1,2,3,4];  // This is a static array
    println!("The first element of the array is {}", a[0]);

    let mut b = [[0u8; 4]; 6];  // array of 6 arrays, of 4 bytes
    b [0] [1] = 42;

    println!("The abs of - 3 is {}", abs(-3));
    println!("2 + 3 is {}", add_three(2));

    for i in a {
        println!("The element is {}", i);
    }

    println!("I'm gonna count reverse from 3:");
    for elem in (1..4).rev(){   // Return a list from 1 to 3 reversed
        println!("{}", elem);
    }

    let tuple_expl : (i32, f64, u8, char) = (500, 6.4, 1, 'a');    // Example of tuple
    let tuple_inf = (500, true); // In this case the type is infered by compiler
    println!("Tuple: {}, {}, {}, {}", tuple_expl.0, tuple_expl.1, tuple_expl.2, tuple_expl.3); // Accessing the elements with . operator
    let (a, b) = tuple_inf; // Unpacking the tuple

    let a: [i32; 6] = [10,20,30,40,50,60]; // Define an array of 6 i32 elements
    let n_a = [3; 5]; // This is an array with 5 3s

    // Statement (let y = ..) done with expression (the scope among {...}) 
    let y = {
        let x = 3;
        x + 1   // NB: w/o semicolon cause it's the returned value
    };

    println!("The value of y is {y}");
}

/*
Custom function to compute abs 
*/
fn abs(x: i32) -> i32 {
    if x >= 0 {
        return x;
    }
    else {
        return -x;
    }
}

// This function returns directly the result w/o return keyward and semicolon
fn add_three(x: i32) -> i32{
    x + 3
}
