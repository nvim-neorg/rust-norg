use rust_norg::parse_tree;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Please provide a file path as a command line argument");
        return;
    }

    let file_path = &args[1];

    let contents = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match parse_tree(&contents) {
        Ok(ast) => println!("parsed ast: {ast:#?}"),
        Err(e) => println!("Couldn't parse :{e:?}, content: {contents}"),
    }
}
