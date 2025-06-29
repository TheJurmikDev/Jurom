use std::fs;
use std::env;
use std::process::Command;
use std::path::Path;
use std::io::{self, Write};

use crate::ast::Token;
use crate::ast::Expression;
use crate::ast::Statement;
mod generator;
mod parsers;
mod lexers;
mod ast;
mod error;
mod project;

use crate::generator::{interpret, transpiler, compiler};
use crate::error::analyzer::Analyzer;
use crate::lexers::lexer;
use crate::project::config::Config;

const RESET: &str = "\x1b[0m";
const BOLD: &str = "\x1b[1m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const RED: &str = "\x1b[31m";
const BLUE: &str = "\x1b[34m";
const DIM: &str = "\x1b[2m";

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage(&args[0]);
        std::process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "create" => {
            if args.len() > 2 {
                eprintln!("{}❌ Usage: {} create{} (do not specify project name after 'create'){}", RED, args[0], RESET, RESET);
                eprintln!("{}💡 For interactive project creation, just use: {} create{}", CYAN, args[0], RESET);
                std::process::exit(1);
            }
            create_project_interactive();
        }
        "run" => {
            if args.len() != 2 {
                eprintln!("{}Usage: {} run{}", RED, args[0], RESET);
                std::process::exit(1);
            }
            run_project();
        }
        "build" => {
            if args.len() != 2 {
                eprintln!("{}Usage: {} build{}", RED, args[0], RESET);
                std::process::exit(1);
            }
            build_project();
        },
        _ => {
            print_usage(&args[0]);
            std::process::exit(1);
        }
    }
}

fn read_input(prompt: &str, default_value: &str, options: Option<&[&str]>) -> String {
    print!("{}? {}{}{}", GREEN, BOLD, prompt, RESET);

    match options {
        Some(opts) => {
            print!("{} (", DIM);
            for (i, opt) in opts.iter().enumerate() {
                if *opt == default_value {
                    print!("{}{}{}", DIM, BOLD, opt);
                } else {
                    print!("{}", opt);
                }
                if i < opts.len() - 1 {
                    print!(" / ");
                }
            }
            print! ("{}{}) {}{}", RESET, DIM, RESET, YELLOW);
        },
        None => {
            print!("{} [default: {}{}{}{}] {}{}", DIM, BOLD, default_value, RESET, DIM, RESET, YELLOW);
        }
    }

    io::stdout().flush().unwrap();

    let mut input = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut input).unwrap();
    let trimmed_input = input.trim().to_string();

    if trimmed_input.is_empty() {
        default_value.to_string()
    } else {
        trimmed_input
    }
}


fn create_project_interactive() {
    println!("{}🧪 Creating new project{}", GREEN, RESET);
    println!();
    
    let default_name = "my-project";
    let default_version = "0.1.0";
    let default_author = "Your Name";
    let default_mode = "compiler";
    let default_transpiler_mode = "cpp";
    let default_src_path = "src/";
    let default_main_file = "main.jr";
    
    let project_name = read_input("Project name:", default_name, None);
    let project_version = read_input("Version:", default_version, None);
    let project_author = read_input("Author:", default_author, None);
    
    let mode_options = &["interpret", "transpiler", "compiler"];
    let project_mode = read_input("Select a mode:", default_mode, Some(mode_options));
    
    let transpiler_mode_options = &["cpp"];
    let project_transpiler_mode = read_input("Select a transpilation mode:", default_transpiler_mode, Some(transpiler_mode_options));
    
    let project_src_path = read_input("Source files path (e.g., src/):", default_src_path, None);
    let project_main_file = read_input("Main file name (e.g., main.jr):", default_main_file, None);

    println!("{}", RESET);
    println!("{}🔋 Creating project '{}' with the following settings:{}", GREEN, project_name, RESET);
    println!("{}  Version: {}{}{}{}", DIM, RESET, YELLOW, project_version, RESET);
    println!("{}  Author: {}{}{}{}", DIM, RESET, YELLOW, project_author, RESET);
    println!("{}  Mode: {}{}{}{}", DIM, RESET, YELLOW, project_mode, RESET);
    println!("{}  Transpiler Mode: {}{}{}{}", DIM, RESET, YELLOW, project_transpiler_mode, RESET);
    println!("{}  Source Path: {}{}{}{}", DIM, RESET, YELLOW, project_src_path, RESET);
    println!("{}  Main File: {}{}{}{}", DIM, RESET, YELLOW, project_main_file, RESET);
    println!();


    if let Err(e) = fs::create_dir(&project_name) {
        eprintln!("{}❌  Error creating project directory: {}{}", RED, e, RESET);
        std::process::exit(1);
    }

    let project_path = Path::new(&project_name);

    let gitignore_content = r#"# Jurom build outputs
output/
*.exe
*.o
*.obj
*.cpp

# IDE files
.vscode/
.idea/
*.swp
*.swo
*~

# OS files
.DS_Store
Thumbs.db
"#;

    if let Err(e) = fs::write(project_path.join(".gitignore"), gitignore_content) {
        eprintln!("{}❌ Error creating .gitignore: {}{}", RED, e, RESET);
        std::process::exit(1);
    }

    let config_content = format!(
        r#"name = "{}"
version = "{}"
author = "{}"
mode = "{}"
transpiler_mode = "{}"
src_path = "{}"
main_file = "{}"
"#,
        project_name, project_version, project_author, project_mode, project_transpiler_mode, project_src_path, project_main_file
    );

    if let Err(e) = fs::write(project_path.join("config.jurom"), config_content) {
        eprintln!("{}❌ Error creating config.jurom: {}{}", RED, e, RESET);
        std::process::exit(1);
    }
    
    let full_src_path = project_path.join(&project_src_path);
    if let Err(e) = fs::create_dir_all(&full_src_path) {
        eprintln!("{}❌ Error creating source directory: {}{}", RED, e, RESET);
        std::process::exit(1);
    }

    let main_content = format!(
        r#"public class {} {{
    function main() {{
        string greeting = "Hello from {}!";
        println(greeting);
        
        num x = 42;
        println("The answer is:");
        println(x);
    }}
}}
"#,
        project_name, project_name
    );

    if let Err(e) = fs::write(full_src_path.join(&project_main_file), main_content) {
        eprintln!("{}❌ Error creating main.jr: {}{}", RED, e, RESET);
        std::process::exit(1);
    }

    println!("{}✅  Project '{}' created successfully!{}", GREEN, project_name, RESET);
    println!("{}📁 Project structure:", YELLOW);
    println!("   {}/", project_name);
    println!("   ├── .gitignore");
    println!("   ├── config.jurom");
    println!("   └── {}", project_src_path);
    println!("       └── {}{}", project_main_file, RESET);
    println!(" ");
    println!("{}💡 To run your project:{}", GREEN, RESET);
    println!("   {}cd {}{}{}{}", DIM, RESET, YELLOW, project_name, RESET);
    println!("   {}jurom {}{}build{}",DIM, RESET, YELLOW, RESET);
    println!("   {}jurom {}{}run{}", DIM, RESET, YELLOW, RESET);
    println!("{}", RESET);
}

fn build_project() {
    if !Path::new("config.jurom").exists() {
        eprintln!("{}❌ No config.jurom found in the current directory.{}", RED, RESET);
        eprintln!("{}💡 Make sure you are in a Jurom project directory, or create a new project using:{}", CYAN, RESET);
        eprintln!("   {}jurom create{}", BOLD, RESET);
        std::process::exit(1);
    }

    let config = match load_config() {
        Ok(config) => config,
        Err(e) => {
            eprintln!("{}❌ Error loading config.jurom: {}{}", RED, e, RESET);
            std::process::exit(1);
        }
    };

    let main_file_path = Path::new(&config.src_path).join(&config.main_file);
    if !main_file_path.exists() {
        eprintln!("{}❌ Main file not found: {}{}", RED, main_file_path.display(), RESET);
        std::process::exit(1);
    }

    println!("{}👷 Building project '{}'...{}", GREEN, config.name, RESET);
    println!("{}📁 Main file: {}{}", BLUE, main_file_path.display(), RESET);
    println!("{}⚙️ Mode: {}{}", BLUE, config.mode, RESET);

    let source = match fs::read_to_string(&main_file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("{}Error reading file {}: {}{}", RED, main_file_path.display(), e, RESET);
            std::process::exit(1);
        }
    };

    let mut lexer = lexer::Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if matches!(token, Token::EOF) {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    let mut parser = parsers::parser::Parser::new(tokens);
    let ast = parser.parse();

    let mut analyzer = Analyzer::new();
    if let Err(errors) = analyzer.analyze(&ast) {
        eprintln!("{}❌ Analysis Errors:{}{}", RED, RESET, RED);
        for error in errors {
            eprintln!("  • {}{}", error, RESET);
        }
        std::process::exit(1);
    }

    match config.mode.as_str() {
        "interpret" => {
            println!("{}🔄 Interpreting mode: copying script...{}", CYAN, RESET);
            let output_dir = Path::new("output").join("interpret");
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);
        }

        "transpiler" => {
            println!("{}🔧 Transpiling to {}...{}", CYAN, config.transpiler_mode, RESET);
            let output_dir = Path::new("output")
                .join("transpiler")
                .join(&config.transpiler_mode);
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            match config.transpiler_mode.as_str() {
                "cpp" => {
                    let mut code_generator = transpiler::cpp::CppCodeGenerator::new();
                    let cpp_code = code_generator.generate(ast);
                    let cpp_filename = output_dir.join(format!("{}.cpp", config.name));
                    if let Err(e) = fs::write(&cpp_filename, &cpp_code) {
                        eprintln!("{}Error writing C++ file: {}{}", RED, e, RESET);
                        std::process::exit(1);
                    }

                    println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);
                    println!("{}📁 Generated C++ code: {}{}", BLUE, cpp_filename.display(), RESET);

                    println!("{}🔨 Compiling to executable...{}", YELLOW, RESET);
                    let exe_name = if cfg!(windows) {
                        output_dir.join(format!("{}.exe", config.name))
                    } else {
                        output_dir.join(config.name.clone())
                    };

                    let compile_result = try_compile_cpp(
                        cpp_filename.to_str().unwrap(),
                        exe_name.to_str().unwrap(),
                    );

                    match compile_result {
                        Ok(()) => {
                            println!("{}✅  Successfully compiled to: {}{}", GREEN, exe_name.display(), RESET);
                            println!("{}📦 All files are in: {}{}", BLUE, output_dir.display(), RESET);
                        }
                        Err(e) => {
                            eprintln!("{}❌ Compilation failed: {}{}", RED, e, RESET);
                            std::process::exit(1);
                        }
                    }
                }
                _ => {
                    eprintln!("{}❌ Unsupported transpiler mode: {}{}", RED, config.transpiler_mode, RESET);
                    std::process::exit(1);
                }
            }
        }

        "compiler" => {
            println!("{}🔧 Compiling with LLVM...{}", CYAN, RESET);
            let output_dir = Path::new("output").join("compiler");
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }
            println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);

            let exe_name = if cfg!(windows) {
                output_dir.join(format!("{}.exe", config.name))
            } else {
                output_dir.join(config.name.clone())
            };

            match compiler::CompilerWrapper::compile_to_exe(ast, exe_name.to_str().unwrap()) {
                Ok(()) => {
                    println!("{}✅  Successfully compiled with LLVM to: {}{}", GREEN, exe_name.display(), RESET);
                    println!("{}📦 Executable is in: {}{}", BLUE, output_dir.display(), RESET);
                }
                Err(e) => {
                    eprintln!("{}❌ LLVM compilation failed: {}{}", RED, e, RESET);
                    std::process::exit(1);
                }
            }
        }

        _ => {
            eprintln!("{}❌ Unsupported mode: {}{}", RED, config.mode, RESET);
            eprintln!("{}💡 Supported modes: interpret, transpiler, compiler{}", CYAN, RESET);
            std::process::exit(1);
        }
    }
}

fn run_project() {
    if !Path::new("config.jurom").exists() {
        eprintln!("{}❌ No config.jurom found in the current directory.{}", RED, RESET);
        eprintln!("{}💡 Make sure you are in a Jurom project directory, or create a new project using:{}", CYAN, RESET);
        eprintln!("   {}jurom create{}", BOLD, RESET);
        std::process::exit(1);
    }

    let config = match load_config() {
        Ok(config) => config,
        Err(e) => {
            eprintln!("{}❌ Error loading config.jurom: {}{}", RED, e, RESET);
            std::process::exit(1);
        }
    };

    let main_file_path = Path::new(&config.src_path).join(&config.main_file);

    if !main_file_path.exists() {
        eprintln!("{}❌ Main file not found: {}{}", RED, main_file_path.display(), RESET);
        std::process::exit(1);
    }

    println!("{}🚀 Running project '{}'...{}", GREEN, config.name, RESET);
    println!("{}📁 Main file: {}{}", BLUE, main_file_path.display(), RESET);
    println!("{}⚙️ Mode: {}{}", BLUE, config.mode, RESET);

    let source = match fs::read_to_string(&main_file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("{}Error reading file {}: {}{}", RED, main_file_path.display(), e, RESET);
            std::process::exit(1);
        }
    };

    let mut lexer = lexer::Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if matches!(token, Token::EOF) {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    let mut parser = parsers::parser::Parser::new(tokens);
    let ast = parser.parse();

    let mut analyzer = Analyzer::new();
    if let Err(errors) = analyzer.analyze(&ast) {
        eprintln!("{}❌ Analysis Errors:{}{}", RED, RESET, RED);
        for error in errors {
            eprintln!("  • {}{}", error, RESET);
        }
        std::process::exit(1);
    }

    match config.mode.as_str() {
        "interpret" => {
            println!("{}🔄 Interpreting...{}", CYAN, RESET);
            let output_dir = Path::new("output").join("interpret");
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }
            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }
            println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);

            println!("{}▶️ Executing interpreted script...{}", YELLOW, RESET);
            let mut interpreter = interpret::interpret::Interpreter::new();
            interpreter.execute(ast);
        }
        "transpiler" => {
            println!("{}🔧 Transpiling to {}...{}", CYAN, config.transpiler_mode, RESET);
            let output_dir = Path::new("output")
                .join("transpiler")
                .join(&config.transpiler_mode);
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            match config.transpiler_mode.as_str() {
                "cpp" => {
                    let mut code_generator = transpiler::cpp::CppCodeGenerator::new();
                    let cpp_code = code_generator.generate(ast);
                    let cpp_filename = output_dir.join(format!("{}.cpp", config.name));
                    if let Err(e) = fs::write(&cpp_filename, &cpp_code) {
                        eprintln!("{}Error writing C++ file: {}{}", RED, e, RESET);
                        std::process::exit(1);
                    }
                    println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);
                    println!("{}📁 Generated C++ code: {}{}", BLUE, cpp_filename.display(), RESET);

                    println!("{}🔨 Compiling to executable...{}", YELLOW, RESET);
                    let exe_name = if cfg!(windows) {
                        output_dir.join(format!("{}.exe", config.name))
                    } else {
                        output_dir.join(config.name.clone())
                    };

                    let compile_result = try_compile_cpp(
                        cpp_filename.to_str().unwrap(),
                        exe_name.to_str().unwrap(),
                    );

                    match compile_result {
                        Ok(()) => {
                            println!("{}✅  Successfully compiled to: {}{}", GREEN, exe_name.display(), RESET);
                            println!("{}📦 All files are in: {}{}", BLUE, output_dir.display(), RESET);
                            println!("{}▶️ Running the generated executable...{}", YELLOW, RESET);

                            let run_result = Command::new(exe_name.to_str().unwrap()).spawn();

                            match run_result {
                                Ok(mut child) => {
                                    let status = child.wait().expect("Failed to wait for child process.");
                                    if status.success() {} else {
                                        eprintln!("{}❌ Executable exited with error code: {:?}{}", RED, status.code(), RESET);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("{}❌ Failed to start executable: {}{}", RED, e, RESET);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("{}❌ Compilation failed: {}{}", RED, e, RESET);
                            std::process::exit(1);
                        }
                    }
                }
                _ => {
                    eprintln!("{}❌ Unsupported transpiler mode: {}{}", RED, config.transpiler_mode, RESET);
                    std::process::exit(1);
                }
            }
        }
        "compiler" => {
            println!("{}🔧 Compiling with LLVM...{}", CYAN, RESET);
            let output_dir = Path::new("output").join("compiler");
            if let Err(e) = fs::create_dir_all(&output_dir) {
                eprintln!("{}Error creating output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }

            let script_copy_path = output_dir.join(format!("{}.jr", config.name));
            if let Err(e) = fs::copy(&main_file_path, &script_copy_path) {
                eprintln!("{}Error copying script to output directory: {}{}", RED, e, RESET);
                std::process::exit(1);
            }
            println!("{}📁 Script copied to: {}{}", BLUE, script_copy_path.display(), RESET);

            let exe_name = if cfg!(windows) {
                output_dir.join(format!("{}.exe", config.name))
            } else {
                output_dir.join(config.name.clone())
            };

            match compiler::CompilerWrapper::compile_to_exe(ast, exe_name.to_str().unwrap()) {
                Ok(()) => {
                    println!("{}✅  Successfully compiled with LLVM to: {}{}", GREEN, exe_name.display(), RESET);
                    println!("{}📦 Executable is in: {}{}", BLUE, output_dir.display(), RESET);
                    println!("{}▶️ Running the compiled executable...{}", YELLOW, RESET);

                    let run_result = Command::new(exe_name.to_str().unwrap()).spawn();

                    match run_result {
                        Ok(mut child) => {
                            let status = child.wait().expect("Failed to wait for child process.");
                            if status.success() {} else {
                                eprintln!("{}❌ Executable exited with error code: {:?}{}", RED, status.code(), RESET);
                            }
                        }
                        Err(e) => {
                            eprintln!("{}❌ Failed to start executable: {}{}", RED, e, RESET);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("{}❌ LLVM compilation failed: {}{}", RED, e, RESET);
                    std::process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("{}❌ Unsupported mode: {}{}", RED, config.mode, RESET);
            eprintln!("{}💡 Supported modes: interpret, transpiler, compiler{}", CYAN, RESET);
            std::process::exit(1);
        }
    }
}

fn load_config() -> Result<Config, String> {
    let content = fs::read_to_string("config.jurom")
        .map_err(|e| format!("Failed to read config.jurom: {}", e))?;

    let mut config = Config {
        name: String::new(),
        version: String::new(),
        author: String::new(),
        mode: String::new(),
        transpiler_mode: String::new(),
        src_path: String::new(),
        main_file: String::new(),
    };

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        if let Some((key, value)) = line.split_once('=') {
            let key = key.trim();
            let value = value.trim().trim_matches('"');

            match key {
                "name" => config.name = value.to_string(),
                "version" => config.version = value.to_string(),
                "author" => config.author = value.to_string(),
                "mode" => config.mode = value.to_string(),
                "transpiler_mode" => config.transpiler_mode = value.to_string(),
                "src_path" => config.src_path = value.to_string(),
                "main_file" => config.main_file = value.to_string(),
                _ => {}
            }
        }
    }

    if config.name.is_empty() {
        return Err("Missing 'name' in config.jurom".to_string());
    }
    if config.version.is_empty() {
        return Err("Missing 'version' in config.jurom".to_string());
    }
    if config.mode.is_empty() {
        return Err("Missing 'mode' in config.jurom".to_string());
    }
    if config.src_path.is_empty() {
        return Err("Missing 'src_path' in config.jurom".to_string());
    }
    if config.main_file.is_empty() {
        return Err("Missing 'main_file' in config.jurom".to_string());
    }

    Ok(config)
}

fn try_compile_cpp(cpp_filename: &str, exe_name: &str) -> Result<(), String> {
    let result = Command::new("g++")
        .args(&["-std=c++17", "-O3", "-o", exe_name, cpp_filename])
        .output();

    match result {
        Ok(output) => {
            if output.status.success() {
                Ok(())
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr);
                let clang_result = Command::new("clang++")
                    .args(&["-std=c++17", "-O3", "-o", exe_name, cpp_filename])
                    .output();

                match clang_result {
                    Ok(clang_output) => {
                        if clang_output.status.success() {
                            Ok(())
                        } else {
                            Err(format!(
                                "{}Both g++ and clang++ failed.\ng++ error: {}\nclang++ error: {}{}", RED,
                                stderr,
                                String::from_utf8_lossy(&clang_output.stderr),
                                RESET
                            ))
                        }
                    }
                    Err(_) => Err(format!(
                        "{}g++ failed and clang++ not available.\ng++ error: {}{}", RED,
                        stderr,
                        RESET
                    )),
                }
            }
        }
        Err(_) => {
            let clang_result = Command::new("clang++")
                .args(&["-std=c++17", "-O3", "-o", exe_name, cpp_filename])
                .output();

            match clang_result {
                Ok(output) => {
                    if output.status.success() {
                        Ok(())
                    } else {
                        Err(format!(
                            "{}clang++ failed: {}{}", RED,
                            String::from_utf8_lossy(&output.stderr),
                            RESET
                        ))
                    }
                }
                Err(_) => Err(format!("{}Neither g++ nor clang++ is available. Please install a C++ compiler.{}", RED, RESET)),
            }
        }
    }
}

fn print_usage(program_name: &str) {
    println!("{}Jurom Programming Language{}", BOLD, RESET);
    println!();
    println!("{}Usage: {}{} <COMMAND>{}", BOLD, program_name, RESET, BOLD);
    println!();
    println!("{}Commands:{}", BOLD, RESET);
    println!("  {}create{}           Create a new Jurom project (interactive)", GREEN, RESET);
    println!("  {}run{}              Run the current project", GREEN, RESET);
    println!("  {}build{}            Build the current project", GREEN, RESET);
    println!();
    println!("{}Examples:{}", BOLD, RESET);
    println!("  {}jurom create{}", CYAN, RESET);
    println!("  {}cd my_project{}", CYAN, RESET);
    println!("  {}jurom build{}", CYAN, RESET);
    println!("  {}jurom run{}", CYAN, RESET);
    println!();
}
