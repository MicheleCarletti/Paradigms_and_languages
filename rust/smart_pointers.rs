use std::ops::Deref;

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

fn main(){
    let x = 5;
    let y = &x; // Classic reference

    assert_eq!(5, x);
    assert_eq!(5, *y);

    let y1 = Box::new(x);   // Smart pointer, Box points to x stored in the heap instead of stack
    assert_eq!(5, *y1);

    let y2 = MyBox::new(x); // Our custom version of Box<T>, implementing the Deref trait
    assert_eq!(5, *y2);
}