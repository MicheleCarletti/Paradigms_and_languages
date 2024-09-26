/*
Stringhe palindromi
*/

fn is_palindrome(s1: &str) -> bool {
    let s2: String = s1.chars().rev().collect();
    if s2 == s1{
        true
    } else {
        false
    }
}

fn main() {
    let s = String::from("abcba");
    let np = String::from("nopal");
    let res = is_palindrome(&s);
    let res2 = is_palindrome(&np);
    println!("{s} è palindroma: {res}");
    println!("{np} è palindroma: {res2}");
}