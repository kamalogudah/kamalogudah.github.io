---
title: 'Day 3: Ziglings Continued'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

Welcome to Day 3 of my 1000 Days of Zig challenge! Today, I'm working through Ziglings. Ziglings is a great resource for getting your feet wet when it comes to learning Zig.

## Getting Ziglings

Ziglings can be accessed at the following link: [Ziglings](https://codeberg.org/ziglings), beside Ziglings I also reviewed [Zig Guide](https://zig.guide/language-basics) where focused on the language basics.

## Strings
String formatting, helps in ensuring a string formats variables in their correct format for instance looking below:
```zig
    std.debug.print("d={u} {s}{s}\n", .{ d, laugh, major_tom });
```
Here `d`, `laugh` and 'major_tom` have been defined earlier. The 'u' and 's' inside the '{} tells the print() function to format the values as a UTF-8 character and UTF-8 strings respectively.

You can write a multiline string in Zig, this involves putting '\\' at the beginning of each line just like a code comment but with backslashes instead:
```zig
     const two_lines =
         \\Line One
        \\Line Two
    ;
```

## If Statements

Zig like most languages supports conditionals such as using the 'if' statements 
```zig
     if (true) {
         ...
    } else {
        ...
     }
```

On comparisons you will find the usual suspects such as:
```zig
    a == b   means "a equals b"
    a < b    means "a is less than b"
    a > b    means "a is greater than b"
    a != b   means "a does not equal b"
```
In other languages any positive integer can be used in an if statements, but in Zig only boolean values are allowed, 1 is not coerced to true, or 0 to false. An if statement can be used as an expression, this is valid in Zig.
```zig
   const foo: u8 = if (a) 2 else 3;
```
## While loops


FizzBuzz is a popular programing example, here is how you create it in Zig using while and if statements:
```zig
pub fn main() void {
    var i: u8 = 1;
    const stop_at: u8 = 16;

    // What kind of loop is this? A 'for' or a 'while'?
    while (i <= stop_at) : (i += 1) {
        if (i % 3 == 0) std.debug.print("Fizz", .{});
        if (i % 5 == 0) std.debug.print("Buzz", .{});
        if (!(i % 3 == 0) and !(i % 5 == 0)) {
            std.debug.print("{d}", .{i});
        }
        std.debug.print(", ", .{});
    }
    std.debug.print("\n", .{});
}
```
Zig also supports while loops, consisting three parts - a condition, a block and a continue expression.

```zig

 var n: u32 = 2;

    // Please use a condition that is true UNTIL "n" reaches 1024:
    while (n < 1024) {
        // Print the current number
        std.debug.print("{} ", .{n});

        // Set n to n multiplied by 2
        n *= 2;
    }
```
While with a continue expression, 'while' statements can have an optional 'continue expression' which runs every time the while loop continues (either at the end of the loop or when an explicit 'continue' is invoked.

```zig

   var foo = 2;
    while (foo < 10) : (foo += 2) {        
        // Do something with even numbers less than 10...
     }
```
While loop with continue statements.
```zig
     while (condition) : (continue expression) {
       if (other condition) continue;
   }
```
Breaking out of a loop in Zig is simple using a "break" statement as shown below:
```zig
    while (condition) : (continue expression) {
         if (other condition) break;

    }
```
When a while loop stops as a result of a break statemnt the continue expression will not execute unlike the case of a continue statement.

## for loops

Zig has for loops which allows one to execute code for each element of an array or slices (will look into this later).
For loops can use break and continue.

```zig
    for (items) |item| {

       // Do something with item
     }
```
Another variation of for loops uses an index of the iteration, in the form below:
```zig
     for (items, 0..) |item, index| {

        // Do something with item and index

    }
```

```zig 
const expect = @import("std").testing.expect;

test "for" {
    //character literals are equivalent to integer literals
    const string = [_]u8{ 'a', 'b', 'c' };

    for (string, 0..) |character, index| {
        _ = character;
        _ = index;
    }

    for (string) |character| {
        _ = character;
    }

    for (string, 0..) |_, index| {
        _ = index;
    }

    for (string) |_| {}
}
```

## What I Learned Today

- Strings in Zig
- Conditional and loops in Zig.


## Looking Ahead

Tomorrow, I'll continue with Ziglings. Stay tuned for Day 4!

**References**

1. [Systems Programming with Zig](https://livebook.manning.com/book/systems-programming-with-zig/welcome/v-3)
2. [Ziglings](https://codeberg.org/ziglings)
3. [Zig guide](https://zig.guide/language-basics/while-loops)
