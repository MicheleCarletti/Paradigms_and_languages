fn divisione(a: i32, b: i32) -> Result<i32, String> {

    // raise an error if division is not allowed
    if b == 0 {
        Err(String::from("Not allowed 0 division"))
    }  else {
        Ok(a/b)
    }

}

fn main() {
    let a = 10;
    let b = 2;
    
    // Call the function and check the result
    match divisione(a, b) {
        Ok(risultato) => println!("{a} / {b} = {risultato}"),
        Err(errore) => println!("Errore: {errore}")
    }
}