/*
Riccerca del massimo in una lista
*/

fn main() {
    let v = vec![10, 50, 2, 6, 25];
    let mut max = &v[0];

    for i in v.iter() { // Same as &v
        if i > max {
            max = i;
        }
    }

    println!("The maximum is {}", max);
}