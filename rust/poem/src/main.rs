use std::env;
use std::process;

use poem::Config;   // Defined in lib.rs

fn main() {
    //let args: Vec<String> = env::args().collect();  // Get input args. Use iterator instead

    /*Define a new Config by build method, if it fails exit with error*/
    let config = Config::build(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing argumets: {err}");   // Print to standard error
        process::exit(1);
    });

    println!("Searching for '{}'", config.query);
    println!("In file {}", config.file_path);

    // Run the logic. If it return an Err() exit
    if let Err(e) = poem::run(config) {
        eprintln!("Application error: {e}");
        process::exit(1);
    }

}


