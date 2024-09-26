/*
Somma ricorsiva
*/

fn sum_r(n: i32) -> i32 {
    if n == 0 {
        0
    } else {
        n + sum_r(n-1)
    }  
}

fn main() {
    let num = 10;
    let res = sum_r(num);
    println!("La somma dei numeri da 1 a {} è {}", num, res);
}