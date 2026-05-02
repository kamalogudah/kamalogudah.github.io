---
title: 'Day 13: Async in Zig Continued'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - async
---

The 13th day of my 1000 days of Zig was spent on going further into Async in Zig using the artile by [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html), in the previous post we looked at the introduction of Async today we go much further into it. 

## Handling Failure in Async
How do we handle failure, when working with Async in Zig? lets change our code so that the first task `try a.await(io);` returns an error:
```zig
fn juicyMain(gpa: Allocator, io: Io) !void {
    var a = io.async(doWork, .{ gpa, io, "hard" });
    var b = io.async(doWork, .{ gpa, io, "on an excuse not to drink Spezi" });

    try a.await(io);
    try b.await(io);
}

fn doWork(gpa: Allocator, io: Io, flavor_text: []const u8) !void {
    // Simulate an error occurring:
    if (flavor_text[0] == 'h') return error.OutOfMemory;

    const copied_string = try gpa.dupe(u8, flavor_text);
    defer gpa.free(copied_string);
    std.debug.print("working {s}\n", .{copied_string});
    io.sleep(.fromSeconds(1), .awake) catch {};
}
```
Running the above code we will have the first try activating, skipping the second await that will be caught by te leak checker. To fix the above issue we need to add awaits, then we do the tries. This will fix the problem as shown in the cde below.

```zig

fn juicyMain(gpa: Allocator, io: Io) !void {
    var a = io.async(doWork, .{ gpa, io, "hard" });
    var b = io.async(doWork, .{ gpa, io, "on an excuse not to drink Spezi" });

    const a_result = a.await(io); // The fix
    const b_result = b.await(io); // The fix

    try a_result;
    try b_result;
}

fn doWork(gpa: Allocator, io: Io, flavor_text: []const u8) !void {
    // Simulate an error occurring:
    if (flavor_text[0] == 'h') return error.OutOfMemory;

    const copied_string = try gpa.dupe(u8, flavor_text);
    defer gpa.free(copied_string);
    std.debug.print("working {s}\n", .{copied_string});
    io.sleep(.fromSeconds(1), .awake) catch {};
}
```
The above code will fail successfully. The error was handled and no resources leaked. But there has to be a better way of fixing this. 

## Cancelation in Async

Async in Zig has the cancellation primitive, as it allows for use of defer, try, and await like normal, this helps fix the above issue while getting an optimal code. Thanks to cancellation, we now get instant results, this will result in the code throwing as error, because the moment that the first task returns an error, the cancels get run.

```zig
fn juicyMain(gpa: Allocator, io: Io) !void {
    var a = io.async(doWork, .{ gpa, io, "hard" });
    defer a.cancel(io) catch {}; // use of cancelation

    var b = io.async(doWork, .{ gpa, io, "on an excuse not to drink Spezi" });
    defer b.cancel(io) catch {}; // use of cancelation

    try a.await(io);
    try b.await(io);
}

fn doWork(gpa: Allocator, io: Io, flavor_text: []const u8) !void {
    // Simulate an error occurring:
    if (flavor_text[0] == 'h') return error.OutOfMemory;

    const copied_string = try gpa.dupe(u8, flavor_text);
    defer gpa.free(copied_string);
    std.debug.print("working {s}\n", .{copied_string});
    io.sleep(.fromSeconds(1), .awake) catch {};
}
```
Using cancel helps prevent leaking resources, making code to run optimally. Cancel has identical semantics as await, however it also requests cancellation. The conditions under which cancellation requests are honored are defined by each I/O implementation. Both cancel and await are idempotent with respect to themselves and each other.

## What I Learned Today

- Handling Failure in Async in Zig.
- Using cancelation in Async in Zig.

## Looking Ahead

Tomorrow I will continue with exploring the article by [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html) to further explore async.

**References**

1. [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html)
