---
title: 'Day 1: Installation and Hello World'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Hello World
  - 1000DaysOfZig
---

Welcome to Day 1 of my 1000 Days of Zig challenge! Today, I'm setting up the environment and writing my first Zig program. Truth be told, this is my fourth month of doing Zig. I started back in December, so take this Day 1 story with a grain of salt.

## Installing Zig

Zig is available on multiple platforms. The easiest way to get started is to download a pre-built binary from the [Zig downloads page](https://ziglang.org/download/).

### On macOS (using Homebrew):

```bash
brew install zig
```

### On Linux:

```bash
# Download the latest master build
wget https://ziglang.org/builds/zig-linux-x86_64-<version>.tar.xz
tar xf zig-linux-x86_64-<version>.tar.xz
export PATH="$PWD/zig-linux-x86_64-<version>:$PATH"
```

### Verify Installation:

```bash
zig version
```

## Hello, World!

Let's write the classic "Hello, World!" program in Zig. Create a file named `hello.zig`:

```zig
const std = @import("std");

pub fn main() void {
    std.debug.print("Hello, World!\n", .{});
}
```

### Running the Program:

```bash
zig run hello.zig
```

Output:
```
Hello, World!
```

## Understanding the Code

Let's break down what's happening:

1. **`const std = @import("std");`** - Imports the Zig standard library and assigns it to the constant `std`
2. **`pub fn main() void`** - Declares the main function that returns nothing (`void`). Zig is still under heavy development. Just recently, [Zig 0.16 was released](https://ziglang.org/download/0.16.0/release-notes.html), which introduced a number of big changes, including [Writergate](https://github.com/ziglang/zig/pull/24329?ref=dailydev).
3. **`std.debug.print()`** - Uses the standard library's debug print function
4. **`.{}`** - An empty anonymous struct literal, used here as the format arguments

## Using Zig's Build System

Zig comes with a powerful build system. Let's create a proper project:

```bash
mkdir hello_world
cd hello_world
zig init-exe
```

This creates a basic project structure with:
- `build.zig` - Build configuration
- `src/main.zig` - Main source file

### Building the Project:

```bash
zig build
```

### Running the Built Executable:

```bash
zig build run
```

## Cross-Compilation

One of Zig's killer features is easy cross-compilation. Let's compile our hello world for Windows from macOS/Linux:

```bash
zig build-exe hello.zig -target x86_64-windows
```

Or for WebAssembly:

```bash
zig build-exe hello.zig -target wasm32-wasi
```

## What I Learned Today

- Zig installation is straightforward
- The language syntax is clean and explicit
- The build system is integrated and powerful
- Cross-compilation is incredibly easy
- No hidden allocations or control flow

## Looking Ahead

Tomorrow, I'll look into Ziglings and do about 10 exercises. 
Stay tuned for Day 2!

**References**

1. [Zig Official Website](https://ziglang.org/)
2. [Zig Documentation](https://ziglang.org/documentation/master/)
3. [Zig Learn](https://ziglearn.org/)
4. [100 days of code](https://www.100daysofcode.com/)
5. [Systems Programming with Zig](https://livebook.manning.com/book/systems-programming-with-zig/welcome/v-3)
6. [Ziglings](https://codeberg.org/ziglings)
