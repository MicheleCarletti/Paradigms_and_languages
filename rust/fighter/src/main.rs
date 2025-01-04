use rand::Rng;
use std::cmp::{max, min};

pub struct Fighter {
    hp: u32,
    dead: bool
}

impl Fighter {
    pub fn new() -> Fighter {
        Fighter {hp: 10, dead: false}
    }

    pub fn fight(&mut self) -> Result<bool, String> {
        if self.dead {
            Err(String::from("The character is dead"))
        } else {
            let n = rand::thread_rng().gen_range(0..10);    // Generate random number in 0 - 9
            if n >= 5 { // The fighter has won
                Ok(true)
            } else {
                self.hp = max(0, self.hp - 3);  // Update the hp
                if self.hp == 0 {   // If the fighter die 
                    self.dead = true;
                    return Err(String::from("The character lost, and die"));
                }
                Ok(false)   // The fighter has lost               
            }
        }
    }

    pub fn heal(&mut self) {
        self.hp = min(self.hp + 2, 10);
        println!("The fighter has been healed, hp: {}", self.hp);
    }

    pub fn is_dead(&self) -> bool {
        self.dead
    }
}

fn main() {
    let mut f = Fighter::new();
    match f.fight() {
        Err(e) => println!("{e}"),
        Ok(res) => {
            if res {
                println!("The fighter has won!");
            } else {
                println!("The fighter has lost");
            }
        }
    }

    if !f.is_dead() {   // If not dead, heal the fighter
        f.heal();
    }
    
}
