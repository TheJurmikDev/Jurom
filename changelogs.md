# 📜 Jurom Language Changelog

Welcome to the Jurom Language changelog! This file tracks all the updates, bug fixes, and new features added to Jurom. Each version includes a summary of what’s changed, so you can stay up to date with the project’s progress. 🌟

## 📋 How to Read This Changelog
- **Versions**: Listed in reverse chronological order (newest first).
- **Sections**:
    - 🆕 **Added**: New features or functionality.
    - 🐛 **Fixed**: Bug fixes or corrections.
    - 🔧 **Changed**: Improvements or updates to existing features.
    - 🗑️ **Removed**: Features or code that were removed.

## [0.1.3] - 2025-05-10
Some fixes and other things.

### 🐛 Fixed
- Support for nested while.
- Added back fixed while function.

## [0.1.2] - 2025-05-08
Another small update with some features.

### 🆕 Added
- Support for multiple strings in string variable.
- Support for adding different variables to string. 
- Better error handling.

## [0.1.1] - 2025-05-07
Small changes I forgot to add and small fixes.

### 🆕 Added
- Moved parser functions to parser folder.
- Added new arithmetic operator: `-`.
- Better variable finder in scope.
- Added \n sequence support for println.
- Added string support while comparing.

## [0.1.0] - 2025-05-01
Initial release of Jurom Language! 🎉 This version introduces the core features of the language, providing a foundation for learning and experimentation.

### 🆕 Added
- Support for `public class` and `function main()` as the program structure.
- Custom function declarations and calls.
- Variable types: `num` (integers and decimals), `string`, and `boolean`.
- Variable declarations and updates.
- Arithmetic operators: `+`, `*`, `/`.
- Comparison operators: `=`, `<`, `>`, `<=`, `>=`, `!=`.
- Control flow: `if`, `else if`, `else` statements.
- Loops: `while` loops for repeated execution.
- Console output with `system.console.println`.
- Rust based parser and runtime for executing `.jr` files.
