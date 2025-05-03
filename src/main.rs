use std::fs;
use std::path::Path;
use crate::parser::parse_program;

mod parser;
mod interpreter;
mod runtime;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 || !args[1].ends_with(".jr") {
        eprintln!("Usage: jurom <file.jr>");
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let code = fs::read_to_string(path).expect("Can't read file");

    match parse_program(&code) {
        Ok(stmts) => {
            if let Err(e) = interpreter::execute(stmts) {
                eprintln!("Error while executing: {}", e);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error while parsing: {}", e);
            std::process::exit(1);
        }
    }
}