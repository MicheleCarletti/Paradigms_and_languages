use std::fs;

fn main() {
    let result = fs::read_to_string("simple_file.txt");
    let content = match result {
        Ok(arg) => arg,
        Err(e) => e.to_string()
    };
    println!("{content}");

}