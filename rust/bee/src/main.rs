use rand::Rng;

#[derive(Debug)]

// Type of bee
enum BeeType{
    Employed,
    Onlooker,
    Scout
}

struct Bee {
    name: String,
    typology: BeeType,
    exp_life: u32,
}
impl Bee {
    fn new(n: &str, t: BeeType, life: u32) -> Bee {
        Bee {name: n.to_string(), typology: t, exp_life: life}
    }

    fn explore(&self) -> Result<u32, String> {
        match self.typology {
            BeeType::Onlooker => Err(String::from("In hive")),
            _ => {
                let n = rand::thread_rng().gen_range(0..10);    // Generate random number in 0 - 9
                if n >= 5 {
                    Ok(n)
                } else {
                    Err(String::from("Food not found"))
                }

            }
        }
    }
}

struct Swarm {
    bees: Vec<Box<Bee>>
}
impl Swarm {
    fn description(&self) {
        for bee in self.bees.iter() {
            println!("{} I'm a {:?} bee, my expected life is {} days", bee.name, bee.typology, bee.exp_life);
        }   
    }
    fn fly(&self) {
        for bee in self.bees.iter() {
            match bee.explore() {
                Err(s) => println!("Bee {}: {}", bee.name, s),
                Ok(n) => println!("Bee {}: food found! Quality {}", bee.name, n),
            }

        }
        
    }
}

fn main() {

    let swarm = Swarm {
        bees: vec! [
            Box::new(Bee::new("ID_1", BeeType::Employed, 10)),
            Box::new(Bee::new("ID_2", BeeType::Onlooker, 20)),
            Box::new(Bee::new("ID_3", BeeType::Scout, 15)),
        ]
    };

    swarm.description();
    swarm.fly();


}

