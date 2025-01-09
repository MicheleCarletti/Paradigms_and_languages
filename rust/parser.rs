use std::iter::Peekable;

#[derive(Debug, Clone)]
enum Expr {
    Add(Box<Expr>, Box<Expr>), // Sum
    Sub(Box<Expr>, Box<Expr>), // Subtraction
    Mul(Box<Expr>, Box<Expr>), // Multiplication
    Div(Box<Expr>, Box<Expr>), // Division
    Pow(Box<Expr>, Box<Expr>), // Power
    Val(f64),                  // Numeric value
}

impl Expr {
    fn eval(&self) -> f64 {
        match self {
            Expr::Val(x) => *x,
            Expr::Add(e1, e2) => e1.eval() + e2.eval(),
            Expr::Sub(e1, e2) => e1.eval() - e2.eval(),
            Expr::Mul(e1, e2) => e1.eval() * e2.eval(),
            Expr::Div(e1, e2) => {
                let denom = e2.eval();
                if denom == 0.0 {
                    panic!("Error: zero division!");
                }
                e1.eval() / denom
            }
            Expr::Pow(e1, e2) => e1.eval().powf(e2.eval()),
        }
    }
}

// Had to specify the mut ref type as an Iterator. The function accepts any valid iterator
fn parse_value<I>(chars: &mut Peekable<I>) -> Expr
where
    I: Iterator<Item = char>
{
    if let Some(&'-') = chars.peek() {
        chars.next();
        match parse_value(chars) {
            Expr::Val(num) => Expr::Val(-num),
            _ => panic!("Error: numeric value expected after '-'"),
        }
    } else if let Some(&'(') = chars.peek() {
        chars.next();
        let expr = parse_expr(chars);
        if chars.next() != Some(')') {
            panic!("Error: missing ')' ");
        }
        expr
    } else {
        let mut num_str = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_numeric() || ch == '.' {
                num_str.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        if num_str.is_empty() {
            panic!("Error: numeric value expected");
        }
        Expr::Val(num_str.parse::<f64>().unwrap())
    }
}

fn parse_pow<I>(chars: &mut Peekable<I>) -> Expr
where
    I: Iterator<Item = char>
{
    let base = parse_value(chars);
    let mut result = base;
    while let Some(&'^') = chars.peek() {
        chars.next();
        let exponent = parse_value(chars);
        result = Expr::Pow(Box::new(result), Box::new(exponent));
    }
    result
}

fn parse_term<I>(chars: &mut Peekable<I>) -> Expr
where
    I: Iterator<Item = char>
{
    let factor = parse_pow(chars);
    let mut result = factor;
    while let Some(&op) = chars.peek() {
        if op == '*' || op == '/' {
            chars.next();
            let next_factor = parse_pow(chars);
            result = match op {
                '*' => Expr::Mul(Box::new(result), Box::new(next_factor)),
                '/' => Expr::Div(Box::new(result), Box::new(next_factor)),
                _ => panic!("Unexpected operator: {}. Only '*' or '/' are allowed at this stage", op),
            };
        } else {
            break;
        }
    }
    result
}

fn parse_expr<I>(chars: &mut Peekable<I>) -> Expr
where
    I: Iterator<Item = char>
{
    let term = parse_term(chars);
    let mut result = term;
    while let Some(&op) = chars.peek() {
        if op == '+' || op == '-' {
            chars.next();
            let next_term = parse_term(chars);
            result = match op {
                '+' => Expr::Add(Box::new(result), Box::new(next_term)),
                '-' => Expr::Sub(Box::new(result), Box::new(next_term)),
                _ => panic!("Unexpected operator: {}. Only '+' or '-' are allowed at this stage", op),
            };
        } else {
            break;
        }
    }
    result
}

fn parse(input: &str) -> Expr {
    let mut chars = input.chars().filter(|&c| !c.is_whitespace()).peekable();
    let expr = parse_expr(&mut chars);
    if chars.peek().is_some() {
        panic!("Error: unexpected characters after parsing");
    }
    expr
}

fn main() {
    use std::io::{self, Write};

    println!("Insert a math expression, ex. 3 + 5 * (2 - 8)");
    let mut input = String::new();
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut input).unwrap();

    let input = input.trim();
    let ast = parse(input);

    println!("AST: {:?}", ast);
    println!("Result: {}", ast.eval());
}
