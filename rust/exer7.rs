/* Implementa una struct Rettangolo con un metodo che ne calcola l'area*/
#[derive(Debug)] 

struct Rettangolo {
    base: i32,
    height: i32
}

impl Rettangolo {
    fn new(b: i32, h: i32) -> Rettangolo {
        Rettangolo {
            base: b,
            height: h
        }
    }

    fn area(&self) -> i32 {
        self.base * self.height
    }

}

fn main() {
    let rect = Rettangolo::new(2, 5);

    let s = rect.area();

    println!("The area of rectangle {rect:?} is {s}");

}