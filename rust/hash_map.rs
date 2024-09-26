use std::collections::HashMap;

fn main() {
    let text = "hello world wonderful world";

    let mut map = HashMap::new();   // New hash map

    /* Spit text by whitespace*/
    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);   // or_insert() returns a mut refernce to the value for the specified key 
        *count += 1;    // Dereference (*) count and increment it
    }

    println!("{map:?}");
}
