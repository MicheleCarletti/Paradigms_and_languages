/*
Lifetime declaration is needed when the compiler is not able to infer it
applying the three Lifetime Elision Rules
*/
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn main() {
    let string1 = String::from("a long string is really long!");

    {
        let string2 = String::from("abc");
        let result = longest(&string1, &string2);
        println!("The longest string is {result}");
    }

}