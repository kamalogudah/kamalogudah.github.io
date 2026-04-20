---
title: 'Day 2: Ziglings Review Exercises 001 - 014'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

Welcome to Day 2 of my 1000 Days of Zig challenge! Today, I'm working through Ziglings. Ziglings is a great resource for getting your feet wet when it comes to learning Zig.

## Getting Ziglings

Ziglings can be accessed at the following link: [Ziglings](https://codeberg.org/ziglings)

A simple hello world in Zig 
```zig 
const std = @import("std");
pub fn main() void {
    std.debug.print("Hello world!\n", .{});
}
```

A more complex hello world in Zig. As mentioned at the time of this writing, Zig is still under heavy development
```zig 
const std = @import("std");

pub fn main(init: std.process.Init) !void {
    try std.Io.File.stdout().writeStreamingAll(init.io, "Hello, World!\n");
}
```
A variation of hello world during Zig 0.15.2 

```zig
const std = @import("std");

pub fn main() !void {
    const stdout_file = std.fs.File.stdout();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll("All your codebase still belong to us!");
    try stdout.flush();
}
```

You can access the Zig standard library documentation locally by running:
```zig 
zig std
```
This will start a local server for you to browse the documentation. At the time of this writing, the link is `http://127.0.0.1:36349/`.

The following line is common in most Zig applications.
```zig 
const std = @import("std");
```
Imports must be declared as constants because they can only be used at comptime rather than runtime. Zig evaluates constant values at compile time.

Looking at the print function below, notice it takes two parameters: one is a string with placeholders '{}', and the next parameter is an anonymous list literal containing the values to be printed.

```zig
  std.debug.print("{} {} {}\n", .{ n, pi, negative_eleven });
```

Arrays are used in Zig, and when Zig can infer the size of the array, you can use '_' for the size. You can also let Zig infer the type of the value so the declaration is less verbose.
```zig
  var foo = [_]u32{ 42, 108, 5423 };
```
Some array operations include concatenating two arrays using '++':
```zig
   const a = [_]u8{ 1,2 };
   const b = [_]u8{ 3,4 };
   const c = a ++ b ++ [_]u8{ 5 }; // equals 1 2 3 4 5
```
You can use '**' to repeat an array as shown below:

```zig
 const d = [_]u8{ 1,2,3 } ** 2; // equals 1 2 3 1 2 3
```
Both '++' and '**' only operate on arrays during comptime. 

In Zig strings are stored as arrays of bytes.

```zig
   const ziggy = "stardust";
   const d: u8 = ziggy[4];
```

## What I Learned Today

- Zig hello world changes over the past two releases.
- The language is still evolving.
- Zig standard library.
- Parts of the print statement.
- Arrays and introduction to strings.

## Looking Ahead

Tomorrow, I'll continue with Ziglings and read a chapter of Systems Programming with Zig. 
Stay tuned for Day 3!

**References**

1. [Systems Programming with Zig](https://livebook.manning.com/book/systems-programming-with-zig/welcome/v-3)
2. [Ziglings](https://codeberg.org/ziglings)
