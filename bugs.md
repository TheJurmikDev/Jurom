# 🐛 Jurom Language Known Bugs

Welcome to the Jurom Language bug tracker! This file lists all known bugs in the Jurom programming language, along with descriptions, examples, and workarounds (if available). Jurom is a simple language built for learning and fun, and we’re working hard to fix these issues to make it even better! Your help in reporting or fixing bugs is greatly appreciated.

## 📋 How to Use This File
- **Bug Entries**: Each bug has a title, description, example (if applicable), and workaround (if known).
- **Status**: Bugs are marked as **Open** (not fixed) or **Fixed** (resolved in a specific version).
- **Changelog**: Fixed bugs are documented in the [Changelogs](changelogs.md) with the version they were resolved in.

## 🐛 Known Bugs

### 1. `if` Statements Inside `while` Loops Evaluate Conditions Incorrectly
**Status**: Open  
**Description**: When an `if` statement is used inside a `while` loop, the condition (e.g., `k == 5`) is evaluated incorrectly, often returning `true` prematurely. This causes the `if` branch to execute too early, leading to unexpected output or loop termination. The issue appears to be in the `evaluate_expr` function’s handling of `Expr::Comparison` or scope management in `runtime.rs`.

**Example**:
```java
public class main {
  function main() {
    system.console.println("Test While with If");
    num k = 0;
    while (k < 10) {
      k = k + 1;
      if (k == 5) {
        system.console.println("k = 5");
      } else {
        system.console.println(k);
      }
    }
  }
}
```

**Expected Output**:
```
Test While with If
1
2
3
4
k = 5
6
7
8
9
```

**Actual Output**:
```
Test While with If
0
```

**Workaround**:
- Avoid using `if` statements inside `while` loops.
- Move `if` statements outside the loop or use multiple `while` loops to achieve the desired logic. For example:
  ```java
  public class main {
    function main() {
      system.console.println("Workaround Example");
      num count = 0;
      while (count < 5) {
        count = count + 1;
        system.console.println(count);
      }
      if (count == 5) {
        system.console.println("Reached count of 5!");
      }
    }
  }
  ```
  **Output**:
  ```
  Workaround Example
  1
  2
  3
  4
  5
  Reached count of 5!
  ```

**Notes**:
- Likely caused by incorrect variable scoping or comparison logic in `evaluate_expr`.
- Planned fix in a future release (tracked in [TODO List](readme.md#-todo-list)).

## 🆕 Reporting New Bugs
Found a bug we don’t know about? 🙀 Please report it:
1. Check if the bug is already listed here or in the [Issues](https://github.com/TheJurmikDev/Jurom-language/issues) page.
2. Open a new issue on GitHub with:
    - A clear title (e.g., “Variable Assignment Fails in Nested Loops”).
    - A description of the bug, including expected vs. actual behavior.
    - A minimal code example that reproduces the bug.
    - Any error messages or output.
