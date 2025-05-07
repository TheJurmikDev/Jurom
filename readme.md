# 🚀 Jurom Language
Jurom is a simple programming language built for learning and fun! It uses easy syntax, inspired by Java, and runs on a lightweight language named Rust. Whether you're a beginner writing your first program or a developer experimenting with a new language, Jurom makes coding easy and enjoyable. Programs are written in `.jr` files and executed with a single command.

## ✨ Features
- 📚 **Classes**: Write programs inside a `public class`, just like Java.
- ⚡ **Main Function**: Every program starts with a `function main()`.
- 🔧 **Custom Functions**: Create and call your own functions.
- 💡 **Variable Types**: Use `num` (for numbers), `string`, and `boolean`.
- 🔄 **Variable Updates**: Change variable values as needed.
- 🔢 **Smart Numbers**: `num` automatically handles integers and decimals.
- ✅ **Conditionals**: Use `if`, `else if`, and `else` for decision-making.
- 🔁 **Loops**: Write `while` loops to repeat actions.
- 🖨️ **Console Output**: Print text, numbers, or booleans with `system.console.println`.
- 🧮 **Operators**: Supports `+`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `!=`.

## 🛠️ Installation
To start using Jurom, you need [Rust](https://www.rust-lang.org/) installed. Follow these steps:

1. **Install Rust**:
    - Visit [rust-lang.org](https://www.rust-lang.org/tools/install) and install Rust.
    - Run `rustup update` to get the latest version.

2. **Clone the Project**:
   ```bash
   git clone https://github.com/TheJurmikDev/Jurom-language
   cd Jurom-language
   ```

3. **Build Jurom**:
   ```bash
   cargo build
   ```

4. **Run a Program**:
   Using cargo run
   ```bash
   cargo run -- myprogram.jr
   ```
   
   Using builded program
   ```bash
   cargo build --release
   jurom.exe myprogram.jr
   ```

### 📝 Example Program
Here’s a sample Jurom program that shows variables, loops, and conditionals:

```java
public class main {
  function main() {
    system.console.println("Starting Jurom Program!");
    num count = 0;
    while (count < 5) {
      count = count + 1;
      system.console.println(count);
    }
    if (count == 5) {
      system.console.println("Reached count of 5!");
    } else {
      system.console.println("Something went wrong!");
    }
    num value = 10;
    while (value < 13) {
      system.console.println(value);
      value = value + 1;
    }
    system.console.println("Program finished!");
  }
}
```

**Output**:
```
Starting Jurom Program!
1
2
3
4
5
Reached count of 5!
10
11
12
Program finished!
```

Save this as `main.jr` and run it.

## ⚙️ How It Works
- 🧩 **Parser**: Reads `.jr` files and turns code into a structure the compiler understands.
- 🏃 **Runtime**: Executes your program, handling variables, loops, and conditionals.
- 🛠️ **Rust Backend**: Built with Rust for fast and reliable performance.
- 📜 **Syntax**: Designed to be familiar to Java users, but simpler for beginners.

## 📈 Current Progress
Jurom is growing! 🌱 Here’s what’s working:
- Classes, functions, and variable declarations.
- Arithmetic and comparison operators.
- `if`, `else if`, `else`, and `while` statements.
- Console printing with `system.console.println`.

## 📘 Notes and information
This project has been started since 2025 May.
- `skidding` is not good for reputation.
- if you ever find same code, look for date of creation.

Every Jurom program needs few things.
- `public class` that’s on the start of file.
- `function main` that’s function under class that’s automatically started.

## 🔮 TODO List
We’re working on making Jurom even better! Here’s what’s planned:
- 🐛 Fix `if` statements inside `while` loops (currently not working as expected).
- ➕ Add new operators like modulo (`%`) and power (`**`).
- ⌨️ Support reading user input from the console.
- 📋 Add arrays or lists for storing multiple values.
- 🛞 Add break function for while loops.
- 📌 Add math operations like `-`.
- 💎 Add `break` function for while loop.
- 💬 Include comments (e.g., `//` for single-line comments).
- 📢 Improve error messages to be clearer and more helpful.
- ⚡ Optimize the compiler for faster program execution.
