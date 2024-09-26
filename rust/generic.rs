fn largest<T: std::cmp::PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];
    for item in list {
        if item > largest {
            largest = item;
        }
    }
    largest
}

fn main() {
    let number_list = vec![34, 50, 100, 65];
    let res = largest(&number_list);
    println!("The largest number in the list is {res}");

    let char_list = vec!['a', 'm', 'q', 'l'];
    let res = largest(&char_list);
    println!("The largest char in the list is {res}");

}