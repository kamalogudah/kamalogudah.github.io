---
title: 'Day 15: Introduction to Zig'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - BookReview
  - Introduction to Zig
---

Today is day 15 of my 1000 days of Zig. I have been looking at some Zig books and decided to start with [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria. Over the coming weeks or months I will go through it and give a highlight on what I have learned.

## Introduction to Zig
Zig considers each Zig file as a separate module. Your main function lives on the `main.zig`, when building an executable program in Zig, you need to declare a main() function, which represents the entrypoint of your program, i.e., where the execution of your program begins. When developing libraries, something to be reused by other applications, the program will start from `root.zig` as the root source file of the library.
## build.zig and build.zig.zon
When you run `zig init` it also creates build.zig and build.zig.zon. build.zig is the build script, which is executed when you run `zig build`. This will execute any steps you have that help build the project.
The other (build.zig.zon) is a JSON-like file, in which you can describe your project, and also declare a set of dependencies of your project that you want to fetch from the internet. This allows for the inclusion of external libraries into your project; it is similar to cargo, npm or rubygems.

## Type definition in Zig

We specify the type of an object or a function argument in Zig by using a colon character (:) followed by the type after the name of this object/function argument. With the expressions a: i32 and b: i32, we know that both a and b arguments have type i32, which is a signed 32 bit integer. In this part, the syntax in Zig is identical to the syntax in Rust, which also specifies types by using the colon character.

## Return and Errors

The main function in Zig can return nothing (void), an unsigned 8-bit integer (u8) value, or an error. In other words, you can write your main() function in Zig to return essentially nothing (void), or, if you prefer, you can also write a more C-like main() function, which returns an integer value that usually serves as a `status code` for the process.
  Errors in Zig are considered values. Hence, when you have a function that might return an error, you need to:
  1. Add exclamation mark to the return type of the function and make it clear that this function might return an error.
  2. They can explicitly handle this error inside the function.

You know about `try` and `catch`? They exist in Zig but they work in a different way. Check out the code below: we do have a try keyword, but we do not have a catch keyword in this code. In Zig, we use the try keyword to execute an expression that might return an error, which, in this example, is the `stdout.print()` expression. With try we execute the expression; if it returns a valid value, we are good. If not, it will unwrap the error value, then return this error from the function and also print the current stack trace to stderr.

```zig
const std = @import("std");
const Writer = std.Io.File.Writer;

pub fn main(init: std.process.Init) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = Writer.init(
        std.Io.File.stdout(),
        init.io,
        &stdout_buffer
    );
    const stdout = &stdout_writer.interface;
    try stdout.print("Hello, {s}!\n", .{"world"});
    try stdout.flush();
}
```
## Other tools
The zig compiler has other tools such as `build-lib` and `build-obj` commands, which work the exact same way as the build-exe command. The only difference is that they compile your Zig modules into a portable C ABI library or into object files, respectively. In the case of the `build-exe` command, a binary executable file is created by the zig compiler in the root directory of your project. 


## What I Learned Today

- Started reviewing [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria

## Looking Ahead

Tomorrow I will continue with exploring [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria. 

**References**

1. [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria
