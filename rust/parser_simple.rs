use std::io;
use std::str::FromStr;

fn parse_and_evaluate(expression: &str) -> Result<f64, String> {
    let tokens: Vec<&str> = expression.split_whitespace().collect();
    if tokens.len() != 3 {
        return Err(String::from("Invalid format. Use: <num> <op> <num>"));
    }

    let left = f64::from_str(tokens[0]).map_err(|_| "First element is not a valid number")?;
    let operator = tokens[1];
    let right = f64::from_str(tokens[2]).map_err(|_| "Second element is not a valid number")?;

    match operator{
        "+" => Ok(left + right),
        "-" => Ok(left - right),
        "*" => Ok(left * right),
        "/" => {
            if right == 0.0 {
                Err(String::from("Zero division not allowed!"))
            } else {
                Ok(left / right)
            }
        }
        _ => Err(String::from("Not supported operator. Use +, -, *, /"))
    }
} 

fn main() {
    let mut input = String::new();
    println!("Insert a simple operation (ex. 10 + 5)");
    // Read from input
    io::stdin()
        .read_line(&mut input)
        .expect("Input error");
    let input = input.trim();   // Removes blank and newline
    // Parse and compute show the results
    match parse_and_evaluate(input) {
        Ok(result) => println!("Result {result}"),
        Err(error) => println!("{error}"),
    }
}