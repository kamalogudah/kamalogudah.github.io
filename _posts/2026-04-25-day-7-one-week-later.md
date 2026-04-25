---
title: 'Day 7:  One week later'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

It has been a week since I started on the challenge of working 1000 days with Zig. 

## Enums
An "enum" is a Zig construct that lets you give names to numeric values and store them in a set. They look a lot like error sets, where a set means the values are unique.
```zig
  const Fruit = enum{ apple, pear, orange };
  const my_fruit = Fruit.apple;
```
Since enums are sets of numbers, you can have the compiler handle the numbering for you, or assign the numbers explicitly using a given type.
```zig
  const Stuff = enum(u8){ foo = 16 };
```
With enums one can get the integer using the built-in function @intFromEnum().
```zig
 const my_stuff: u8 = @intFromEnum(Stuff.foo);
```
## Structs
Zig has the struct or structure functionality, where we may have this 
```zig
point1_x = 3;
point1_y = 16;
point1_z = 27;
point2_x = 7;
point2_y = 13;
point2_z = 34;
```
Which through a struct can be changed into 
```zig
point1 = Point{ .x=3, .y=16, .z=27 };
point2 = Point{ .x=7, .y=13, .z=34 };
```
The struct above can be defined as shown below:
```zig
const Point = struct{ x: u32, y: u32, z: u32 };
```

Through structs one is able to mimic the class functionality in Object Oriented Programming languages.
```zig
const std = @import("std");

const Role = enum {
    wizard,
    thief,
    bard,
    warrior,
};

const Character = struct {
    role: Role,
    gold: u32,
    health: u8,
    experience: u32,
};

pub fn main() void {
    var chars: [2]Character = undefined;

    // Glorp the Wise
    chars[0] = Character{
        .role = Role.wizard,
        .gold = 20,
        .health = 100,
        .experience = 10,
    };
    // Zump the Loud

    chars[1] = Character{
         .role = Role.bard,
         .gold = 10,
         .health = 100,
         .experience = 20,
     };

    for (chars, 0..) |c, num| {
        std.debug.print("Character {} - G:{} H:{} XP:{}\n", .{
            num + 1, c.gold, c.health, c.experience,
        });
    }
}
```
Above is an example showing usage of a struct and enum courtesy of [Ziglings](https://codeberg.org/ziglings). Running the above without the chars[1], you will get values seeming like garbage. In Zig debug mode, Zig will write the repeating pattern "10101010" in binary (or 0xAA in hex) to all undefined locations to make them easier to spot when debugging.

## What I Learned Today

- Enums and Structs in Zig

## Looking Ahead

Tomorrow is Sunday; I will see what to work on as I embark on my 8th day doing Zig in my 1000-day journey.

**References**

1. [Ziglings](https://codeberg.org/ziglings)
