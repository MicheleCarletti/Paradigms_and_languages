/* Verifica se un numero è primo*/

fn main() {
    let num = 7;
    println!("{num} is prime: {}", is_prime(&num));
}

fn is_prime(x: &i32) -> bool{
    if *x <= 1 {
        return false
    }
    for i in 2..x-1 {
        if x % i == 0 {
            return false
        }
    }
    return true
}