---
title: 'Day 9: Optionals in Zig'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

The 9th day of my 1000 days of Zig was spent on Ziglings.

## Optionals
Optionals in Zig come in handy in cases where our variables may or may not hold a value. Optionals use the syntax ?T and are used to store the data null, or a value of type T. Zig expresses this as follows:
```zig
    var foo: ?u32 = 10;
```
This means that foo can store a u32 integer or null.
```zig
 foo = null;
 if (foo == null) beginToComplain();
```
To use an optional value as the non-null type (a u32 integer in this case), we need to guarantee that it isn't null. One way to do this is to use the "orelse" statement.
 ```zig 
  var bar = foo orelse 2;
```
This means that bar will either equal the u32 integer value stored in foo, or it will equal 2 if foo was null.

Example of optional using tests in Zig, this is from [zig.guide](https://zig.guide/language-basics/optionals):
```zig
test "optional" {
    var found_index: ?usize = null;
    const data = [_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 12 };
    for (data, 0..) |v, i| {
        if (v == 10) found_index = i;
    }
    try expect(found_index == null);
}
```
## Optionals and error unions
Optionals in Zig are a lot like error union types which can either hold a value or an error. Likewise, the orelse statement is like the catch statement used to "unwrap" a value or supply a default value:
```zig
  var maybe_bad: Error!u32 = Error.Evil;
 var number: u32 = maybe_bad catch 0;
```

## Optional Types with Structs
Optional types can be applied to structs, in the last article we were linking the three elephants together, but did not consider a case of a tail not pointing to another elephant. Using the previous example we can decide to make the elephant tails optional as shown below:

```zig
const Elephant = struct {
    letter: u8,
    tail: ?*Elephant = null, // making tail optional
    visited: bool = false,
};
```

Further in the main function we can have this:
```zig

pub fn main() void {
    var elephantA = Elephant{ .letter = 'A' };
    var elephantB = Elephant{ .letter = 'B' };
    var elephantC = Elephant{ .letter = 'C' };

    // Link the elephants so that each tail "points" to the next.
    linkElephants(&elephantA, &elephantB);
    linkElephants(&elephantB, &elephantC);

    // `linkElephants` will stop the program if you try and link an
    // elephant that doesn't exist! Uncomment and see what happens.
    // const missingElephant: ?*Elephant = null;
    // linkElephants(&elephantC, missingElephant);

    visitElephants(&elephantA);

    std.debug.print("\n", .{});
}

// If e1 and e2 are valid pointers to elephants,
// this function links the elephants so that e1's tail "points" to e2.
fn linkElephants(e1: ?*Elephant, e2: ?*Elephant) void {
    e1.?.tail = e2.?;
}

// This function visits all elephants once, starting with the
// first elephant and following the tails to the next elephant.
fn visitElephants(first_elephant: *Elephant) void {
    var e = first_elephant;

    while (!e.visited) {
        std.debug.print("Elephant {u}. ", .{e.letter});
        e.visited = true;

        // We should stop once we encounter a tail that
        // does NOT point to another element. Here we break from the loop 

        e = e.tail orelse break;
    }
}

```

## What I Learned Today

- Use of optionals in Zig

## Looking Ahead

Tomorrow I will continue with Ziglings, getting deeper into it, and learning more new things about Zig.

**References**

1. [Ziglings](https://codeberg.org/ziglings)
