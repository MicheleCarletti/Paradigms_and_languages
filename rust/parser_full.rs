use std::io;

fn evaluate(expression: &str) -> Result<f64, String> {
    // Convert to postfix notation (RPN)
    let rpn = infix_to_postfix(expression)?;
    // Evaluate postifx expression
    evaluate_postfix(&rpn)
}

fn infix_to_postfix(expression: &str) -> Result<Vec<String>, String> {
    let mut output = Vec::new();
    let mut operators = Vec::new();

    let tokens = tokenize(expression)?;

    for token in tokens {
        if let Ok(_) = token.parse::<f64>() {
            output.push(token); // Push numbers directly in output
        } else if token == "(" {
            operators.push(token);
        } else if token == ")" {
            // Extract until "("
            while let Some(op) = operators.pop() {
                if op == "(" {
                    break;
                }
                output.push(op);
            }

        } else {
            // Operators
            while let Some(op) = operators.last() {
                if precedence(op) >= precedence(&token) && op != "(" {
                    output.push(operators.pop().unwrap());
                } else {
                    break;
                }
            }
            operators.push(token);
        }
    }
    // Add missing operators
    while let Some(op) = operators.pop(){
        if op == "(" || op == ")" {
            return Err(String::from("Not balanced parenthesis"));
        }
        output.push(op);
    }
    Ok(output)
}

fn evaluate_postfix(rpn: &[String]) -> Result<f64, String> {
    let mut stack = Vec::new();

    for token in rpn {
        if let Ok(num) = token.parse::<f64>() {
            stack.push(num);
        } else {
            let b = stack.pop().ok_or("Invalid expression")?;
            let a = stack.pop().ok_or("Invalid expression")?;
            stack.push(match token.as_str() {
                "+" => a + b,
                "-" => a - b,
                "*" => a * b,
                "/" => {
                    if b == 0.0 {
                        return Err(String::from("Zero division!"))
                    } 
                    a / b
                }
                "^" => a.powf(b),
                _ => return Err(format!("Operator not valid: {}", token)),
            });
        }
    }

    if stack.len() != 1 {
        Err(String::from("Invalid expression"))
    } else {
        Ok(stack.pop().unwrap())
    }
}

fn tokenize(expression: &str) -> Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut num_buf = String::new();

    for c in expression.chars() {
        if c.is_whitespace() {
            continue;
        } else if c.is_digit(10) || c == '.' {
            num_buf.push(c);
        } else {
            if !num_buf.is_empty() {
                tokens.push(num_buf.clone());
                num_buf.clear();
            }
            if "+-*/^()".contains(c) {
                tokens.push(c.to_string());
            } else {
                return Err(format!("Character not valid: {c}"));
            }
        }
    }
    if !num_buf.is_empty() {
        tokens.push(num_buf);
    }

    Ok(tokens)
}

fn precedence(op: &str) -> i32 {
    match op {
        "+" | "-" => 1,
        "*" | "/" => 2,
        "^" => 3,
        _ => 0,
    }
}

fn main() {
    println!("Insert a math expression ex. 2 + (3 * 4)");

    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Input error");
    let input = input.trim();

    match evaluate(input) {
        Ok(result) => println!("Result {result}"),
        Err(error) => println!("{error}"),
    }
}