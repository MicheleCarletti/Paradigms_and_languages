/* Problema della torre di Hanoi:
spostare i dischi dal piolo di partenza a quello di arrivo usando il piolo intermedio,
mettendo i dischi in ordine crescente*/

fn hanoi(n: u32, from: char, to: char, aux: char) {
    if n == 1 {
        println!("Sposta il disco 1 dal piolo {} al piolo {}", from, to);
    } else {
        // Sposta n-1 dischi dal piolo 'from' al piolo 'aux' usando il piolo 'to' come ausiliario
        hanoi(n-1, from, aux, to);
        // Sposta l'ultimo disco (il più grande) dal piolo 'from' al piolo 'to'
        println!("Sposta il disco {} dal piolo {} al piolo {}", n, from, to);
        // Sposta n-1 dischi dal piolo 'aux' al piolo 'to' usando il piolo 'from' come ausiliario
        hanoi(n-1, aux, to, from);
    }
}

fn main() {
    let n = 3; // Numero di dischi
    println!("Soluzione del problema di Hanoi per {} dischi", n);
    hanoi(n, 'A', 'C', 'B');    // 'A' piolo di partenza, 'C' piolo di arrivo, 'B' piolo ausiliario
}