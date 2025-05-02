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
    let code = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path.display(), e);
            std::process::exit(1);
        }
    };

    let stmts = match parse_program(&code) {
        Ok(stmts) => stmts,
        Err(e) => {
            eprintln!("Parsing error: {}", e);
            std::process::exit(1);
        }
    };

    if let Err(e) = interpreter::execute(stmts) {
        eprintln!("Execution error: {}", e);
        std::process::exit(1);
    }
}