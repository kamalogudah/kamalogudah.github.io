---
title: 'Day 8: Pointers in Zig'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

Today is the eighth day of trying to do 1000 continuous days of learning Zig.

## Pointers
A pointer is a reference to a value. In the example below, bar is a reference to the memory space currently containing the value 5.
```zig
    var foo: u8 = 5;      // foo is 5
    var bar: *u8 = &foo;  // bar is a pointer
```
A pointer cheatsheet based on the above definition of a pointer.
```zig
// A cheatsheet given the above declarations:
//     u8         the type of a u8 value
//     foo        the value 5
//     *u8        the type of a pointer to a u8 value
//     &foo       a reference to foo
//     bar        a pointer to the value at foo
//     bar.*      the value 5 (the dereferenced value "at" bar)
```
## Variable and Constant Pointers

Consider the following:
```zig
     var foo: u8 = 5;
    const bar: u8 = 5;
```
The above means that 
```zig
     &foo is of type "*u8"
    &bar is of type "*const u8"
```
You can make a const pointer to a mutable value (var), but you cannot make a var pointer to an immutable value (const). Meaning once a value is declared immutable, you can't coerce it to a mutable type. A pointer's mutability (var vs const) refers to the ability to change what the pointer points to, not the ability to change the value at that location!
```zig
    const locked: u8 = 5;
    var unlocked: u8 = 10;

     const p1: *const u8 = &locked;
     var   p2: *const u8 = &locked;
```
From the above both p1 and p2 point to constant values which cannot change. However, p2 can be changed to point to something else and p1 cannot!
```zig
    const p3: *u8 = &unlocked;
    var   p4: *u8 = &unlocked;
   const p5: *const u8 = &unlocked;
     var   p6: *const u8 = &unlocked;
```

In this case p3 and p4 can both be used to change the value they point to but p3 cannot point at anything else. What's interesting is that p5 and p6 act like p1 and p2, but point to the value at "unlocked". This is what we mean when we say that we can make a constant reference to any value.

## Passing Values by Reference

You can use pointers to pass a value by reference to a function. Why would we want to pass a pointer to an integer variable rather than the integer value itself? Because this allows us to change the value of the variable. In a nutshell, use pass-by-reference when you want to change the pointed-to value. Otherwise, pass the value.
An example courtesy of  [Ziglings](https://codeberg.org/ziglings):
```zig
const std = @import("std");

pub fn main() void {
    var num: u8 = 1;
    var more_nums = [_]u8{ 1, 1, 1, 1 };

    // Let's pass the num reference to our function and print it:
    makeFive(&num);
    std.debug.print("num: {}, ", .{num});

    // Now something interesting. Let's pass a reference to a
    // specific array value:
    makeFive(&more_nums[2]);

    // And print the array:
    std.debug.print("more_nums: ", .{});
    for (more_nums) |n| {
        std.debug.print("{} ", .{n});
    }

    std.debug.print("\n", .{});
}

// This function should take a reference to a u8 value and set it
// to 5.
fn makeFive(x: *u8) void {
    x.* = 5; // fix me!
}
``` 
## Pointers with Structs

You can pass a pointer to a struct when you need to modify that struct. Pointers are also useful when you need to store a reference to a struct (a "link" to it).
```zig
    const Vertex = struct{ x: u32, y: u32, z: u32 };

     var v1 = Vertex{ .x=3, .y=2, .z=5 };
    var pv: *Vertex = &v1;   // <-- a pointer to our struct
// Note that you don't need to dereference the "pv" pointer to access
// the struct's fields:
//
//     YES: pv.x
//     NO:  pv.*.x
```

## Functions taking Pointers to Structs as arguments
In Zig we can write functions that take pointers to structs as arguments. This foo() function modifies struct v:
```zig
     fn foo(v: *Vertex) void {
         v.x += 2;
        v.y += 3;
         v.z += 7;
     }
```

The above function definition can be called as below:
```zig
    foo(&v1);
```
An example using the above is as shown below:
```zig
const std = @import("std");

const Class = enum {
    wizard,
    thief,
    bard,
    warrior,
};

const Character = struct {
    class: Class,
    gold: u32,
    health: u8 = 100, // You can provide default values
    experience: u32,

    // I need to use the '?' here to allow for a null value. But

    mentor: ?*Character = null,
};

pub fn main() void {
    var mighty_krodor = Character{
        .class = Class.wizard,
        .gold = 10000,
        .experience = 2340,
    };

    var glorp = Character{ // Glorp!
        .class = Class.wizard,
        .gold = 10,
        .experience = 20,
        .mentor = &mighty_krodor, // Glorp's mentor is the Mighty Krodor
    };

    printCharacter(&glorp);
}

// Note how this function's "c" parameter is a pointer to a Character struct.
fn printCharacter(c: *Character) void {
    // Here's something you haven't seen before: when switching an enum, you
    // don't have to write the full enum name. Zig understands that ".wizard"
    // means "Class.wizard" when we switch on a Class enum value:
    const class_name = switch (c.class) {
        .wizard => "Wizard",
        .thief => "Thief",
        .bard => "Bard",
        .warrior => "Warrior",
    };

    std.debug.print("{s} (G:{} H:{} XP:{})\n", .{
        class_name,
        c.gold,
        c.health,
        c.experience,
    });

    // Checking an "optional" value and capturing it will be
    // explained later (this pairs with the '?' mentioned above.)
    if (c.mentor) |mentor| {
        std.debug.print("  Mentor: ", .{});
        printCharacter(mentor);
    }
}
```
A further example showing usage of pointers in Zig
```zig
//    "Elephants walking
//     Along the trails
//
//     Are holding hands
//     By holding tails."
//
//     from Holding Hands
//       by Lenore M. Link
//
const std = @import("std");

const Elephant = struct {
    letter: u8,
    tail: *Elephant = undefined,
    visited: bool = false,
};

pub fn main() void {
    var elephantA = Elephant{ .letter = 'A' };
    var elephantB = Elephant{ .letter = 'B'};
    var elephantC = Elephant{ .letter = 'C' };

    // Link the elephants so that each tail "points" to the next elephant.
    // They make a circle: A->B->C->A...
    elephantA.tail = &elephantB;
    elephantB.tail = &elephantC;
    elephantC.tail = &elephantA;

    visitElephants(&elephantA);

    std.debug.print("\n", .{});
}

// This function visits all elephants once, starting with the
// first elephant and following the tails to the next elephant.
// If we did not "mark" the elephants as visited (by setting
// visited=true), then this would loop infinitely!
fn visitElephants(first_elephant: *Elephant) void {
    var e = first_elephant;

    while (!e.visited) {
        std.debug.print("Elephant {u}. ", .{e.letter});
        e.visited = true;
        e = e.tail;
    }
}
```


## What I Learned Today

- Use of pointers in Zig

## Looking Ahead

Tomorrow I may go back to the book "System Programming with Zig". That will be day 9 of my journey.

**References**

1. [Ziglings](https://codeberg.org/ziglings)
