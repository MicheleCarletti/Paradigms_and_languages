/* Borrowing lunghezza stringa*/

fn main() {
    let x = String::from("I'm a string");
    let l = str_len(&x);
    println!("La lunghezza della stringa '{}' è {l} ", x);

}

fn str_len(s: &str) -> usize {
    s.len()
}