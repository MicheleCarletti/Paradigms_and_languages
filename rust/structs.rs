struct User {
    active: bool,
    username: String,
    email: String,
    sign_in_count: u64,
}

fn main() {
    let user1 = User{
        active: true,
        username: String::from("MicheleC"),
        email: String::from("michelecarletti98@gmail.com"),
        sign_in_count: 1,
    };

    println!("{}", user1.username);

    let user2 = build_user(String::from("user@example.com"), String::from("user123"));

    /* Create a new user keeping most of the fields from a previous instance*/
    let user3 = User {
        email: String::from("user3@example.com"),
        ..user1 // All other fields are the ones of user1
    };

    // NB: user1 is no more available since email (String) is moved to user3
    
    let user4 = User {
        username: String::from("user4"),
        email: String::from("user4@example.com"),
        ..user2
    };

    // NB: user2 is still available since actve (bool) and sign_in_count (u64) are Copy type
}

fn build_user(email: String, username: String) -> User {
    User {
        active: true,
        username,
        email,
        sign_in_count: 1,
    }
}