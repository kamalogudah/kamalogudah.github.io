---
title: 'Day 11: Many item pointers'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
  - pointers
---

The 11th day of my 1000 days of Zig was spent on the Zig guide, reviewing some of the articles on the page.

## Many item pointers
Most programs in Zig need to keep track of buffers which don't have compile-time known lengths. Many-item pointers are used for these. These act similarly to their single-item counterparts, using the syntax `[*]T` instead of `*T`.
Many-item pointers differ from single-item pointers in the following major ways:
1. Single-item pointers are dereferenceable, e.g., `ptr.*`, while many-item pointers are not.
2. Many-item pointers are indexable, unlike single-item pointers. With many-item pointers, we can access elements using `ptr[0]`.
3. Single-item pointers support arithmetic, e.g., `ptr + 1` or `ptr - 1`, while many-item pointers do not.
4. Single-item pointers allow for any size, including unknown, while many-item pointer sizes must be known.
5. Lastly, while many-item pointers allow for coercion from an array pointer, single-item pointers do not.
 
In this example code, we've written a function that can take in a buffer of any length. Notice how a single-item pointer to an array of bytes coerces into a many-item pointer of bytes.
```zig

fn doubleAllManypointer(buffer: [*]u8, byte_count: usize) void {
    var i: usize = 0;
    while (i < byte_count) : (i += 1) buffer[i] *= 2;
}

test "many-item pointers" {
    var buffer: [100]u8 = [_]u8{1} ** 100;
    const buffer_ptr: *[100]u8 = &buffer;

    const buffer_many_ptr: [*]u8 = buffer_ptr;
    doubleAllManypointer(buffer_many_ptr, buffer.len);
    for (buffer) |byte| try expect(byte == 2);

    const first_elem_ptr: *u8 = &buffer_many_ptr[0];
    const first_elem_ptr_2: *u8 = @ptrCast(buffer_many_ptr);
    try expect(first_elem_ptr == first_elem_ptr_2);
}
```
We can convert from a many-item pointer to a single-item pointer by either indexing an element and dereferencing that, or by using @ptrCast to cast the pointer type. This is only valid when the buffer has a length of at least 1.

## What I Learned Today

- many-item pointers in Zig

## Looking Ahead

Tomorrow I will continue with Ziglings, getting deeper into it, and learning more new things about Zig.

**References**

1. [Ziglings](https://codeberg.org/ziglings)
2. [Many item Pointers](https://zig.guide/language-basics/many-item-pointers)
