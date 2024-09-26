#[derive(Debug)]

pub enum Vegetable {
    Asparagus,
    Tomato,
    Carrot,
    Aubergine,
}

pub fn color(veg: &Vegetable) -> String{
    match veg {
        Vegetable::Asparagus => String::from("green"),
        Vegetable::Tomato => String::from("red"),
        Vegetable::Carrot => String::from("orange"),
        Vegetable::Aubergine => String::from("purple"),
    }
}