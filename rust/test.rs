fn main() {
    let mut x = 5;

    {
        let y = &x;
        // x += 1   // Not allowed cause x is borrowed by y
        println!("{y}");
    }

    x += 1; // It's allowed cause y is no longer valid
    println!("x = {}", x);
}