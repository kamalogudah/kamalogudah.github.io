---
title: 'Day 17: Introduction to Zig : Primitive data types'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - BookReview
  - Introduction to Zig
---

## Primitive Data Types
Zig has many different primitive data types available for you to use, an elaborate list is on the Zig website. Among the types available include: 

1. Unsigned integers: u8, `8-bit integer`; u16, `16-bit integer`; u32, `32-bit integer`; u64, `64-bit integer`; u128, `128-bit integer`.
2. Signed integers: i8, `8-bit integer`; i16, `16-bit integer`; i32, `32-bit integer`; i64, `64-bit integer`; i128, `128-bit integer`.
3. Float number: f16, `16-bit floating point`; f32, `32-bit floating point`; f64, `64-bit floating point`; f128, `128-bit floating point`;
4. Boolean: bool, represents true or false values.
5. C ABI compatible types: c_long, c_char, c_short, c_ushort, c_int, c_uint, and many others.
6. Pointer sized integers: isize and usize.

## Arrays
To create an array in Zig you have to specify its size (how many elements will it hold) inside a pair brackets, this is followed by the data type of the elements in the array. `All elements present in an array in Zig must have the same data type`. 

One can use the underscore character (_) in place of size as follows:
```zig
const ns = [4]u8{48, 24, 12, 6};
const ls = [_]f64{432.1, 87.2, 900.05};
_ = ns; _ = ls;
```
Using the (_) informs the compiler to fill the field with the number of elements listed in the curly braces. One is technically leaving the role of counting number of elements to the compiler.

Zig's arrays are static, they cannot grow in size once declared. This is a common practice in low level programming as the developer always gets almost full memory control, and expansion of arrays is related to memory management.

## Array methods
Zig has a number of array actions that one can perform one being selecting a particular item in the array, this is done by putting the index of the item in brackets after the object name. Below we are selecting the third element from the ns array defined above. Zig is 0 indexed.
```zig
try stdout.print("{d}\n", .{ ns[2] }); // 12
try stdout.flush();
``
One can select specific slices from an array using the range selector. The range selector has the folowing syntax `start..end`, the `end tail` of the range selector is non-inclusive, meaning that, the index at the end is not included in the range that is selected from the array, in essence the syntax means the following `start..end - 1`.

You can for example, create a slice that goes from the first to the last elements of the array, by using ar[0..ar.len] syntax In other words, it’s a slice that accesses all elements in the array.

``` zig 
const ar = [4]u8{48, 24, 12, 6};
const sl = ar[0..ar.len];
_ = sl;
```

You can also use the syntax `start..` in your range selector. Which tells the zig compiler to select the portion of the array that begins at the start index until the last element of the array. In the example below, we are selecting the range from index 1 until the end of the array.
```zig
const ns = [4]u8{48, 24, 12, 6};
const sl = ns[1..];
_ = sl;
```

## What I Learned Today

- Continued working on [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria

## Looking Ahead

Tomorrow I will continue with exploring [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria. 

**References**

1. [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria
