use std::fs;
use std::path::Path;
use crate::parser::parse_program;
use colored::*;

mod parser;
mod interpreter;
mod runtime;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 || !args[1].ends_with(".jr") {
        eprintln!("{}", "Usage: jurom <file.jr>".red().bold());
        std::process::exit(0);
    }

    let path = Path::new(&args[1]);
    let code = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("{}: {}", "Error reading file".red().bold(), e);
        std::process::exit(0);
    });

    match parse_program(&code) {
        Ok(stmts) => {
            if let Err(e) = interpreter::execute(stmts) {
                e.print(&code);
                std::process::exit(0);
            }
        }
        Err(e) => {
            e.print(&code);
            std::process::exit(0);
        }
    }
}