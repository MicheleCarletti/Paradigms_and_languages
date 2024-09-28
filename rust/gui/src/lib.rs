/* Define a trait Draw for evvery drawnable object*/
pub trait Drawn {
    fn draw(&self);
}

/* This object contains a vector of different drawnable oobjects*/
pub struct Screen {
    pub components: Vec<Box<dyn Drawn>>,
}


impl Screen {
    // Draw all objects in the screen
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}
/* Provide a button*/
pub struct Button {
    pub width: u32,
    pub height: u32,
    pub label: String,
}

impl Drawn for Button {
    fn draw(&self) {
        // Code to draw the button
        println!("I'm a button!");
    }
}
