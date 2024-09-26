use std::io;

fn main(){
    let a = [1, 2, 3, 4, 5];

    println!("Enter an arry index");

    let mut index = String::new();

    io::stdin().read_line(&mut index).expect("Failed to read line");

    let index: usize = index.trim().parse().expect("Index enterd was not a number");    // From string to usize

    if index <= a.len() {
        let element = a[index];
        println!("The value at index {index} is: {element}");
    } else {
        println!("Invalid index value!");
    }

    /* Vectors */
    let v = vec![1, 2, 3]; // Vec<i32> initialized

    let mut w: Vec<i32> = Vec::new();  // Define a new Vec<i32>

    w.push(1);
    w.push(2);
    w.push(3);

    let sc = &v[1];
    println!("The second element of v is {sc}");

    assert_eq!(v, w);

    let third: Option<&i32> = v.get(2);  // get() returns an Option<T>

    match third {
        Some(third) => println!("The third element of v is {third}"),
        None => println!("There is no third element in v"),
    }

    for i in &mut w {
        *i += 10;
    }

    for j in &w {
        println!("{j}");
    }
    

}