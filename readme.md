# 🚀 Jurom Programming Language

> **A modern, minimalist programming language that has modern solution**

Jurom is a clean, Java / Python inspired programming language designed for simplicity and performance. With its elegant syntax and triple execution modes, Jurom bridges the gap between rapid prototyping and high-performance compiled code.

## ✨ Features

### 🎯 **Core Language Features**
- **Object-Oriented Programming** - Classes, methods, and encapsulation
- **Static Typing** - `num` for numbers, `string` for text
- **Control Flow** - `if/else` statements and `while` loops
- **Variables & Expressions** - Full arithmetic and comparison operations
- **Method Chaining** - Clean, readable code with dot notation

### ⚡ **Triple Execution Modes**
- **🔧 Transpiler Mode** - Transpiles to optimized C++ code, then compiles to native executable
- **🚀 Interpreter Mode** - Direct execution for rapid development and testing
- **👷 Compiler Mode** - Compiles project code to highly optimized native executable

---

## 🚀 Quick Start

### Installation
1. **Clone the repository**
   ```bash
   git clone https://github.com/TheJurmikDev/Jurom.git
   cd jurom
   ```

2. **Build the compiler**
   ```bash
   cargo build --release
   ```

### Set up your project
1. **Create new project**
   ```bash
   ./jurom.exe create
   cd my_project
   ```

2. **Project structure**
   ```text
   my_project/
   ├── src/
   │   └── main.jr          # Complete language implementation
   ├── .gitignore           # Ignore file for github
   └── config.jurom         # Config of the project
   ```

3. **Edit main file** 
   ```java
    public class my_project {
        function main() {
            string greeting = "Hello, Jurom!";
            println(greeting);
        
            num count = 1;
            while (count < 5) {
                println(count);
                count = count + 1;
            }
        }
    }
    ```

4. **Run your project**
    ```bash
    ./jurom.exe run
    ```

## 🎯 Language Capabilities

### ✅ **Currently Supported**
- [x] **Class Declarations** with `public class ClassName`
- [x] **Function Definitions** with `function functionName()`
- [x] **Variable Declarations** with `num` and `string` types
- [x] **Assignments** with `=` operator
- [x] **Arithmetic Operations** (`+`, `-`, `*`, `/`)
- [x] **Comparison Operations** (`==`, `<`, `>`, `!=`, `>=`, `<=`, `&&`, `||`)
- [x] **Conditional Statements** (`if/else`)
- [x] **Loops** (`while`)
- [x] **Method Calls** with dot notation
- [x] **Console Output** via `println()`
- [x] **String Literals** with double quotes
- [x] **Numeric Literals** (integers)
- [x] **Binary Expressions** with proper precedence
- [x] **Variable References** in expressions

### 🔀 **Interpretation Process**
1. **Lexical Analysis** – Source code → Tokens
2. **Parsing** – Tokens → Abstract Syntax Tree (AST)
3. **Semantic Analysis** – Checking correctness of syntax and types
4. **Interpretation** – AST → Executing code at runtime

### 🔄 **Transpilation Process**
1. **Lexical Analysis** - Source code → Tokens
2. **Parsing** - Tokens → Abstract Syntax Tree (AST)
3. **Semantic Analysis** – Checking correctness of syntax and types
4. **Code Generation** - AST → Optimized C++ code
5. **Native Compilation** - C++ code → Platform-specific executable
6. 
### ⏩ **Compilation Process**
1. **Lexical Analysis** - Source code → Tokens
2. **Parsing** - Tokens → Abstract Syntax Tree (AST)
3. **Semantic Analysis** – Checking correctness of syntax and types
4. **Code Generation** - AST → Optimized Jurom code
5. **Native Compilation** - Juom code → Platform-specific executable

### 🎨 **Architecture Highlights**
- **Rust-Powered** - Memory-safe, fast compilation
- **Recursive Descent Parser** - Clean, maintainable parsing logic
- **AST-Based** - Structured intermediate representation
- **Modular Design** - Separate lexer, parser, interpreter, and code generator

## 📈 Performance

Jurom's interpret mode runs code interactively via a Rust based runtime:
- **Low Overhead**: Efficient AST evaluation in Rust.
- **Memory Efficient**: Variables in `HashMap` with Rust's stack based locals.
- **Fast Execution**: Leverages Rust's speed for responsive performance.

Jurom's transpiler mode generates optimized C++ code with:
- **Zero Runtime Overhead** - Direct C++ compilation
- **Memory Efficiency** - Stack-allocated variables
- **Native Speed** - Full compiler optimizations (-O3)

Jurom's compiler mode generates optimized code with:
- **Extreme Optimization** - Uses agresive optimization while compiling
- **Static Binary Output** – Produces standalone executables with no dynamic linking overhead
- **Scalable Memory Model** – Efficient stack allocation with support for larger code and data sections

## 🛣️ Roadmap

### 🎯 **Planned Features**
- [ ] **Arrays and Collections**
- [ ] **File I/O Operations**
- [ ] **Function Parameters**
- [ ] **Return Values**
- [ ] **String Interpolation**
- [ ] **Error Handling**
- [ ] **Module System**
- [ ] **Standard Library Expansion**

## 📗 ChangeLogs

You can find all changelogs in [ChangeLogs](changelog.md).

## 📄 License

This project is open source and available under the [GNU License](LICENSE).

---

**Made with ❤️ for developers who appreciate clean, fast languages.**

*Jurom - Where simplicity meets performance.*

### 😎 Made By TheJurmik | Visit offical site https://thejurmik.dev