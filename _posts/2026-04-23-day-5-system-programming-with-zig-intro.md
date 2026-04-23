---
title: 'Day 5: System Programming with Zig Introduction'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

It is day 5 of my 1000 Days of Zig challenge! Today I looked at System Programming with Zig the first chapter and later worked through some Ziglings exercises. 

## Not a C replacement
Zig, unlike the new crop of system programming languages, does not aim to replace C. Its main focus is to complement C, acknowledging the decades of existence of C. It aims to modernize the already existing C code by allowing for new solutions to be extended using Zig. 
## No unused variables
Zig does not allow unused variables and parameters; one has to explicitly discard them in the code using `_`. The only way valid Zig code can contain unused variables is when the code shows that this was intentional. Also, one cannot discard a variable when it is actually being used.

```zig
const x = 5;
_ = x; // Discards x
```
Discarded function parameter:
```zig
fn callback(_: i32) void {
    // Parameter is ignored
}
```

## const vs var
In Zig, variables can be a `const` or `var` for immutable and mutable variables respectively. Changing a const variable results in an error, as well as not changing a var variable. This makes Zig code easy to understand for both the compiler and the person reading the code.

```zig
const x = 5;//immutable
var y = 50; //mutable
y = 46; // mutating y
```

## Error Handling

As covered yesterday, errors and return values are to be handled or discarded explicitly. Remember an error is a value in Zig. Programs in Zig specify the entire set of errors a function may produce, and this is used as an error union return type.

```zig
const MyNumberError = error{
    TooBig,
    TooSmall,
    TooFour,
};
```
## Explicit Types
Zig has an explicit type system, where types are restrictive especially when it comes to numeric, where one will have limited operations they can do based on the type under operation. This makes the developer to choose the types being used in their programs with great consideration. This limits errors that results from unexpected type coercion and casting.

## Comptime
Comptime is Zig's take on meta programming, though slightly different from mainstream languages—be they systems or interpreted languages like Ruby or Python. Comptime allows for code to be run during the compile time, resulting in great performance and code optimization during runtime. 

## Memory 

Allocators, allocators—in Zig one has a way of manually managing memory. You will notice a lot of allocators in most Zig code; this allows you to make use of both the stack and the heap when needed.

## Defer and Errdefer
Zig allows you to have some part of the code run after the code exits its bounding block, through the `defer` keyword. 
```zig
{
    defer runLater();
    runNow();
}
```
This results in, `runLater()` running when the block ({...}) has finished in this order.
```zig
   runNow();
   runLater();
```

Due to an error you may have a code block that could exit in a number of places, but one may need to perform an action before the exit, this is where `errdefer` comes in:
```zig
// An "errdefer" is a defer that only runs if the block exits with an error:
{
    errdefer cleanup();
    try canFail();
}
```
Here the cleanup() function is called ONLY if the "try" statement returns an error produced by canFail().

## What I Learned Today

- Why Zig
- Error handling in Zig and why it is different.
- Defer and errdefer.
- Const and var.


## Looking Ahead

Tomorrow, and over the weekend I will continue with Ziglings and hopefully read more on Systems Programming with Zig, may also watch one or more talks. Stay tuned for Day 6!

**References**

1. [Systems Programming with Zig](https://livebook.manning.com/book/systems-programming-with-zig/welcome/v-3)
2. [Ziglings](https://codeberg.org/ziglings)
