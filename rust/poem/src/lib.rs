use std::error::Error;
use std::fs;
use std::env;

/* It contains the query and the file path*/
pub struct Config {
    pub query: String,
    pub file_path: String,
    pub ignore_case: bool,
}

impl Config {
    // Parse the input agrs from terminal, to get query and file path
    pub fn build(mut args: impl Iterator<Item = String> ) -> Result<Config, &'static str> { // Get an interator instead a reference to [String]
        /*
        // If the number of arguments is not enough
        if args.len() < 3 {
            return Err("not enough arguments") // Raise an error
        }

        let query = args[1].clone();
        let file_path = args[2].clone();
        */
        args.next();    // Ignore first element (program name)
        
        let query = match args.next() {
            Some(arg) => arg,
            None => return Err("Didn't get a query string"),
        };

        let file_path = match args.next() {
            Some(arg) => arg,
            None => return Err("Didn't get a file path"),
        };

        // The environment variable should be written in the terminal like: IGNORE_CASE = 1 cargo run -- query file.txt
        let ignore_case = env::var("IGNORE_CASE").is_ok();  // Check whether the environment variable IGNORE_CASE is set, returns true in that case 

        Ok(Config {query, file_path, ignore_case,})
    }
    
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>>  {   // Return a result: () in case Ok, and a Box implementing Error trait in case of error

    // Read file content
    let contents = fs::read_to_string(config.file_path)?; // ? will return the error value from the current function for the caller to handle

    let results = if config.ignore_case {
        search_case_insensitive(&config.query, &contents)
    } else {
        search(&config.query, &contents)
    };

    for line in results {
        println!("{line}");
    }

    Ok (()) // If everything is ok return a null a Result with nothing: ()

}

fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    /*
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.contains(query) {
            results.push(line);
        }
    }
    results
    */

    // Use iterator instead
    contents
        .lines()
        .filter(|line| line.contains(query))
        .collect()
}

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();    // query is now a String non more a string slice
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.to_lowercase().contains(&query) {   // Make it case insensitive
            results.push(line);
        }
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn case_sensitive() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Duct tape.";
        assert_eq!(vec!["safe, fast, productive."], search(query, contents));
    }

    #[test]
    fn case_insensitive() {
        let query = "rUsT";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Trust me.";
        assert_eq!(vec!["Rust:", "Trust me."], search_case_insensitive(query, contents));
    }
}