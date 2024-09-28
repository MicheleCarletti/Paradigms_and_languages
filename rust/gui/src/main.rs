use gui::Drawn;

/* Custom drawnable object*/
struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Drawn for SelectBox {
    fn draw(&self) {
        // Code to draw a SelectBox
        println!("I'm a SelectBox");
    }
}

use gui::{Button, Screen};

fn main() {
    let screen = Screen {
        components: vec![
            Box::new(SelectBox {
                width: 75,
                height: 10,
                options: vec![
                    String::from("Yes"),
                    String::from("Maybe"),
                    String::from("No"),
                ],
            }),
            Box::new(Button {
                width: 50,
                height: 10,
                label: String::from("OK"),
            }),
        ],
    };

    screen.run();
}
