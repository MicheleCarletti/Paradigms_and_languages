use std::io;

fn main() {
    println!("Guess a number between 0 an 10\n");
    
    loop {

        println!("\nPlease insert your guess.");

        let mut guess = String::new();  // New string mutable variable
        // Read from stdin and save into guess
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read the line!");
    
        let guess: i32 = guess.trim().parse().expect("Please type a number!");  // Shadowing on the previous variable, remove spaces (trim) parse it as i32
        
        let (a, _) = rand_int(0, 10, time_seed());  // Generate a random number between 0 and 10
        let res = result(guess, a); // Check the result
        if res {
            println!("Congratulation your guess is correct!");
            break;  // If win stop
        } else {
            println!("Sorry, it was {}", a);
        }

    }


}

fn rand_int(nmin: i32, nmax: i32, seed: u32) -> (i32, u32) {
    // Pseudo-random generator based on xorshift RNG
    let mut seed: u32 = seed;
    seed ^= seed << 13;
    seed ^= seed >> 17;
    seed ^= seed << 5;
    let range = (nmax + 1 - nmin) as u32;
    let val = nmin + (seed % range) as i32;
    (val, seed)
}

fn time_seed() -> u32 {
    // get as seed time elapsed from 1970/01/01 in ms
    use std::time::SystemTime as st;
    let now = st::now().duration_since(st::UNIX_EPOCH).unwrap();
    now.as_millis() as u32

}

fn result(val: i32, rnd: i32) -> bool {
    if val == rnd {
        return true;
    }else {
        return false;
    }
}