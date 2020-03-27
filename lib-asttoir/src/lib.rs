use std::env;
use std::fs;
static FILENAME: &str = "ast.txt";

pub fn hello() {
    println!("Hello, world!");
}

pub fn ReadFromFile() -> std::string::String {
    // Read in Sample
    println!("In file {}", FILENAME);

    let contents = fs::read_to_string(FILENAME).expect("Something went wrong reading the file");

    println!("With text:\n");
    return contents;
}

// Program::new()

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
