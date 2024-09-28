use std::ops::Deref;
use std::rc::Rc;

/* Define a custom Box<T>*/
struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

/* Implement the Deref trait to dereference our custom smart pointer*/
impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {  // Returns a reference to the only elemnent in MyBox
        &self.0
    }
} 

fn hello(x: &str) {
    println!("Hello, {x}");
}

struct CustomSmartPointer {
    data: String,
}
impl Drop for CustomSmartPointer {  // Implementation of Drop trait for CustomSmartPointer
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer with data {}", self.data);
    }
}

/* Define a Cons List usinc Rc<T> so it can have multiple owners*/
enum List {
    Cons(i32, Rc<List>),
    Nil,
}
use crate::List::{Cons, Nil};


fn main(){
    let x = 5;
    let y = &x; // Classic reference

    assert_eq!(5, x);
    assert_eq!(5, *y);

    let y1 = Box::new(x);   // Smart pointer, Box points to x stored in the heap instead of stack
    assert_eq!(5, *y1);

    let y2 = MyBox::new(x); // Our custom version of Box<T>, implementing the Deref trait
    assert_eq!(5, *y2);

    let m = MyBox::new(String::from("Rust"));
    hello(&m);

    let c = CustomSmartPointer {
        data: String::from("my stuff"),
    };

    let d = CustomSmartPointer {
        data: String::from("other stuff"),
    };

    println!("CustomSramtPointers created.");

    let e = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));  // [5, 10, Nil]
    println!("count after creating e = {}", Rc::strong_count(&e));  
    let f = Cons(3, Rc::clone(&e)); // [3, 5, 10, Nil]
    println!("count after creating f = {}", Rc::strong_count(&e));
    {
        let g = Cons(4, Rc::clone(&e)); // [4, 5, 10, Nil]
        println!("count after creating g = {}", Rc::strong_count(&e));
    }
    println!("count after g goes out of scope = {}", Rc::strong_count(&e));
    
}