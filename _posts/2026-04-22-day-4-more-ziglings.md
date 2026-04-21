---
title: 'Day 4: Ziglings from Exercises 020 - 026'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

Welcome to Day 4 of my 1000 Days of Zig challenge! Still working through Ziglings. Ziglings is a great resource for getting your feet wet when it comes to learning Zig.

## Functions 
Functions form the basis of making any programming language usable. They allow for reusing logic which results in development of simple to complicated programs. All function arguments in Zig are viewed as consts, hence they are immutable. Zig functions are camelCase unlike variables which take the snake_case format. An example function: 
```zig
  fn foo(n: u8) u8 {
    return n + 1;
  }
```

## Errors in Zig
Unlike other programming languages, Zig treats errors as values. We name errors so as to easily identify things when something breaks. Zig creates errors as error sets (collection of named errors). 
```zig
const MyNumberError = error{
    TooBig,
    TooSmall,
    TooFour,
};
```
## Error Unions
A function can return a value or error, considering Zig views errors as values. Error unions allow us to factor a case where a function returns an error or the value. Consider this: 
```zig
  var text: Text = getText("foo.txt");
```
The file `foo.txt` may be missing, in Zig we handle this using "error union", this is a value that is either a regular value or error from set. 
```zig
  var text: MyErrorSet!Text = getText("foo.txt");
```

## Catch
What do we do with a result that may be an error union? Catch allows us to "catch" an error, replacing it with a sensible default. 
```zig
   foo = canFail() catch 6;
```

When `canFail()` fails, foo will now equal 6.

After we catch an error we can perform more actions to it, as shown below:
```zig

     canFail() catch |err| {
         if (err == FishError.TunaMalfunction) {
             ...
        }
     };
```

## try

The catch and try in Zig are far from what you are used to in languages like Ruby or Javascript. Zig uses `try` as a syntactic sugar in place of this:
```zig
     canFail() catch |err| return err;
```
In Zig we can convert the above into this sugared version.
```zig
   try canFail();
```
This is especially useful for cases where the catch is an error being returned.

## Hello World Revisited
Hello world in Zig will always leave one with more questions than answers. The real hello world is not like:
```zig
const std = @import("std");

pub fn main() void {
    std.debug.print("Hello, World!\n", .{});
}
```
The real hello world is shown below, which uses standard out instead of standard error, and considers the case where failure or an error may happen. Example below is from Ziglings. 
```zig
const std = @import("std");

// Take note that this main() definition now returns "!void" rather
// than just "void". Since there's no specific error type, this means
// that Zig will infer the error type.
//
// You can find more information at:
// https://ziglang.org/documentation/master/#Inferred-Error-Sets
//
pub fn main(init: std.process.Init) !void {
    // Instance for input/output operations; we will learn more about this later.
    const io = init.io;

    // We get a Writer for Standard Out...
    var stdout_writer = std.Io.File.stdout().writer(io, &.{});
    // ...and extract its interface so we can print() to it.
    const stdout = &stdout_writer.interface;

    // Unlike std.debug.print(), the Standard Out writer can fail
    // with an error. We don't care _what_ the error is, we want
    // to be able to pass it up as a return value of main().
    //
    // We just learned of a single statement which can accomplish this.
    try stdout.print("Hello world!\n", .{});
}
```
That is it for today.

## What I Learned Today

- Functions in Zig
- Error handling in Zig and why it is different.
- Errors are values.
- Hello world revisited.


## Looking Ahead

Tomorrow, I'll continue with Ziglings and hopefully check out some other resources like Systems Programming with Zig. Stay tuned for Day 5!

**References**

1. [Systems Programming with Zig](https://livebook.manning.com/book/systems-programming-with-zig/welcome/v-3)
2. [Ziglings](https://codeberg.org/ziglings)
3. [Zig guide](https://zig.guide/language-basics/while-loops)
