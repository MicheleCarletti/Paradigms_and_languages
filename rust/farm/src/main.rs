use farm::{Dog, Cat, Pig, Farm};

/* Define a new animal*/
use farm::Animal;

struct Sheep {
    name: String
}
impl Sheep {
    fn new(name: &str) -> Sheep {
        Sheep {name: name.to_string()}
    }
}
impl Animal for Sheep {
    fn speak(&self) {
        println!("I'm {} Sheep. I say BEE!", self.name);
    }
}

fn main() {
    // Define a farm with a dog a cat two pigs and a sheep
    let myfarm = Farm {
        animals: vec! [
            Box::new(Dog::new("Danny")),
            Box::new(Cat::new("Candy")),
            Box::new(Pig::new("Peppa")),
            Box::new(Pig::new("George")),
            Box::new(Sheep::new("Suzy")),
        ]
    };

    myfarm.run();   // Run the farm
}
