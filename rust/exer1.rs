/*
FizzBuzz: Scrivi un programma che stampi i numeri da 1 a 100. 
Per i multipli di 3, stampa "Fizz", per i multipli di 5 stampa "Buzz", e per i multipli di entrambi stampa "FizzBuzz".
*/

fn main() {

    for i in 1..101 {
        if i % 3 == 0 && i % 5 != 0 {
            println!("Fizz {i}");
        }
        if i % 3 != 0 && i % 5 == 0 {
            println!("Buzz {i}");
        }
        if i % 3 == 0 && i % 5 == 0 {
            println!("FizzBuzz {i}");
        }
    }
}