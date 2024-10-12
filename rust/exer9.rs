/* Media, mediana e moda di una lista di numeri*/

use std::collections::HashMap;

fn main() {
    let mut x = vec![10,20,30,40,40,10,20];

    let media = calcola_media(&x);
    let mediana = calcola_mediana(&mut x);
    let moda = calcola_moda(&x);

    println!("La media di x è {media}");
    println!("La mediana di x è {mediana}");
    println!("La moda di x è {moda}");
}

fn calcola_media(v: &Vec<i32>) -> f64 {
    let somma: i32 = v.iter().sum();
    somma as f64 / v.len() as f64
}

fn calcola_mediana(v: &mut Vec<i32>) -> f64 {
    v.sort();
    let len = v.len();
    if len % 2 == 0 {
        (v[len/2 - 1] + v[len/2]) as f64 / 2.0
    }else {
        v[len/2] as f64
    }
}

fn calcola_moda(v: &Vec<i32>) -> i32 {
    let mut frequenza = HashMap::new();
    for &num in v {
        *frequenza.entry(num).or_insert(0) += 1;
    }
    let mut moda = v[0];
    let mut max_frequenza = 0;

    for (num, &count) in &frequenza {
        if count > max_frequenza {
            max_frequenza = count;
            moda = *num;
        }
    }
    
    moda
}

