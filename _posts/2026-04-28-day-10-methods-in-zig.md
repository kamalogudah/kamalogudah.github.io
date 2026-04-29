---
title: 'Day 10: Zig methods'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
  - methods
---

The 10th day of my 1000 days of Zig was spent on Ziglings looking at methods.

## Methods
In Zig you can attach functions to stucts alongside other type definitions. 
```zig
  const Foo = struct{
      pub fn hello() void {
        std.debug.print("Foo says hello!\n", .{});
      }
  };
```
Having a function within a struct namespaces it within that struct, and to call it you have tp specify the namespace and a dot syntax as shown below:
```zig
  Foo.hello();
```

If the first argument of the function is an instance of the struct or pointer to one, we can use the instance as the namespace instead of the type.
```zig
    const Bar = struct{
        pub fn a(self: Bar) void {}
        pub fn b(this: *Bar, other: u8) void {}
         pub fn c(bar: *const Bar) void {}
    };

   var bar = Bar{};
   bar.a() // is equivalent to Bar.a(bar)
    bar.b(3) // is equivalent to Bar.b(&bar, 3)
    bar.c() // is equivalent to Bar.c(&bar)
```
The name of the parameter does not matter, you can use self, lower version of the type name.
Example in code
```zig
const Alien = struct {
    health: u8,

    // We hate this method:
    pub fn hatch(strength: u8) Alien {
        return Alien{
            .health = strength * 5,
        };
    }
};

// Your trusty weapon. Zap those aliens!
const HeatRay = struct {
    damage: u8,

    // We love this method:
    pub fn zap(self: HeatRay, alien: *Alien) void {
        alien.health -= if (self.damage >= alien.health) alien.health else self.damage;
    }
};

```
Usage of the above is as follows:
```zig
pub fn main() void {
    // Look at all of these aliens of various strengths!
    var aliens = [_]Alien{
        Alien.hatch(2),
        Alien.hatch(1),
        Alien.hatch(3),
        Alien.hatch(3),
        Alien.hatch(5),
        Alien.hatch(3),
    };

    var aliens_alive = aliens.len;
    const heat_ray = HeatRay{ .damage = 7 }; // We've been given a heat ray weapon.

    // We'll keep checking to see if we've killed all the aliens yet.
    while (aliens_alive > 0) {
        aliens_alive = 0;

        // Loop through every alien by reference (* makes a pointer capture value)
        for (&aliens) |*alien| {

            // *** Zap the alien with the heat ray here! ***
            heat_ray.zap(alien);

            // If the alien's health is still above 0, it's still alive.
            if (alien.health > 0) aliens_alive += 1;
        }

        std.debug.print("{} aliens. ", .{aliens_alive});
    }

    std.debug.print("Earth is saved!\n", .{});
}
```
Going back to the elephant example we saw in [Pointers in Zig](https://kamalogudah.github.io/zig/day-8-pointers-in-zig/) we can further improve on the code by adding some methods to the Elephant  struct.
```zig
const std = @import("std");

const Elephant = struct {
    letter: u8,
    tail: ?*Elephant = null,
    visited: bool = false,

    // New Elephant methods!
    pub fn getTail(self: *Elephant) *Elephant {
        return self.tail.?; // Remember, this means "orelse unreachable"
    }

    pub fn hasTail(self: *Elephant) bool {
        return (self.tail != null);
    }

    pub fn visit(self: *Elephant) void {
        self.visited = true;
    }

    pub fn print(self: *Elephant) void {
        // Prints elephant letter and [v]isited
        const v: u8 = if (self.visited) 'v' else ' ';
        std.debug.print("{u}{u} ", .{ self.letter, v });
    }
};

pub fn main() void {
    var elephantA = Elephant{ .letter = 'A' };
    var elephantB = Elephant{ .letter = 'B' };
    var elephantC = Elephant{ .letter = 'C' };

    // This links the elephants so that each tail "points" to the next.
    elephantA.tail = &elephantB;
    elephantB.tail = &elephantC;

    visitElephants(&elephantA);

    std.debug.print("\n", .{});
}

// This function visits all elephants once, starting with the
// first elephant and following the tails to the next elephant.
fn visitElephants(first_elephant: *Elephant) void {
    var e = first_elephant;

    while (true) {
        e.print();
        e.visit();

        // This gets the next elephant or stops:
        // which method do we want here?
        e = if (e.hasTail()) e.??? else break;
    }
}

```

Zig enums also have methods, the same way as we have it for structs.

## What I Learned Today

- Use of methods in Zig.

## Looking Ahead

Tomorrow I will continue with Ziglings, getting deeper into it, and learning more new things about Zig. Example of methods being [used inside an enum](https://github.com/ziglang/zig/blob/6787f163eb6db2b8b89c2ea6cb51d63606487e12/lib/std/debug.zig#L477).


**References**

1. [Ziglings](https://codeberg.org/ziglings)
2.
