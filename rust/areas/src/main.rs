#[derive(Debug)]    // This attribute makes Rectangle struct implent Debug. We can display it using println!() or dbg!()
struct Rectangle {
    width: u32,
    height: u32,
}

/* Implementing methods for the struct Rectangle*/
impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }

    /* Constuctor for squared Rectangle*/
    fn square_rect(size: u32) -> Self { // This is not a method but an associated function (does not have self as parameter)
        Self {
            width: size,
            height: size,
        }
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    let rect2 = Rectangle {
        width: 10,
        height: 40,
    };

    let rect3 = Rectangle {
        width: 60,
        height: 45,
    };

    let sq_rect = Rectangle::square_rect(3); // Instantiate a squared Rectangle of size 3

    println!("The area of rect1 {rect1:?} is {} square pixels", rect1.area());    // :? defines the output format: Debug

    println!("Can rect1 {rect1:?} hold rect2 {rect2:?} ? {}", rect1.can_hold(&rect2));
    println!("Can rect1 {rect1:?} hold rect3 {rect3:?} ? {}", rect1.can_hold(&rect3));
    println!("I'm a square {sq_rect:?}");
}

