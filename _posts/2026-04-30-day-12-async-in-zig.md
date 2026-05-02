---
title: 'Day 12: Async in Zig'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - async
---

The 12th day of my 1000 days of Zig was spent on looking at Async in Zig I am planning to start on my first actual Zig project a web crawler *wish me luck*.

## Async
Asynchronous means that things can happen independently of the main program flow. According to [this nodejs article ](https://nodejs.org/learn/asynchronous-work/javascript-asynchronous-programming-and-callbacks#asynchronicity-in-programming-languages) in the current consumer computers, every program runs for a specific time slot and then it stops its execution to let another program continue their execution. This thing runs in a cycle so fast that it's impossible to notice. We think our computers run many programs simultaneously, but this is an illusion (except on multiprocessor machines).

"Async" makes it possible for programming code to run independently, letting the program continue executing other tasks while waiting for long-running operations—like data fetching or file I/O—to finish. It prevents the application from locking up, improving performance by handling multiple tasks concurrently without waiting for each to complete.

Zig reintroduced [async in version 0.16] (https://andrewkelley.me/post/zig-new-async-io-text-version.html) after previously removing it. 

## Why Async 
I am planning to use async in my web crawler project that I will be building soon, hence the reason for taking a look at it. 

Using the example from [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html) on Zig async we have the following example which is not yet using async.

```zig
const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

fn juicyMain(gpa: Allocator, io: Io) !void {
    _ = gpa;

    doWork(io);
}

fn doWork(io: Io) void {
    std.debug.print("working\n", .{});
    io.sleep(.fromSeconds(1), .awake) catch {};
}

pub fn main() !void {
    // Set up allocator.
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer assert(debug_allocator.deinit() == .ok);
    const gpa = debug_allocator.allocator();

    // Set up our I/O implementation.
    var threaded: std.Io.Threaded = .init(gpa);
    defer threaded.deinit();
    const io = threaded.io();

    return juicyMain(gpa, io);
}

```
 Above you notice our setting up a std.Io implementation is a lot like setting up an allocator. This is done once, in main(), and then  the instance is passed throughout the application. Any reusable code accepts an allocator when it need to allocate, or accepts and  needs to accept an Io parameter if it needs to perform I/O operations.
 The above implmentation uses Io that is threads based. In the above code doWork is being called, and it in turn just calling sleep().

 ## Adding Async/await

 Making changes to the juicyMain() and doWork() methods we have this:
 ```zig
 fn juicyMain(gpa: Allocator, io: Io) !void {
     _ = gpa;
 
     var future = io.async(doWork, .{io});
 
     future.await(io); // idempotent
 }
 
 fn doWork(io: Io) void {
     std.debug.print("working\n", .{});
     io.sleep(.fromSeconds(1), .awake) catch {};
 }
 ```
 Which makes use of async/await to call doWork. In Zig async/await means to decouple the calling of the function to the returning of the function.

 ## Do two things at once
To start doing things at once we have the following code:
 
 ```zig

 fn juicyMain(gpa: Allocator, io: Io) !void {
     _ = gpa;
 
     var a = io.async(doWork, .{ io, "hard" });
     var b = io.async(doWork, .{ io, "on an excuse not to drink Spezi" });
 
     a.await(io);
     b.await(io);
 }
 
 fn doWork(io: Io, flavor_text: []const u8) void {
     std.debug.print("working {s}\n", .{flavor_text});
     io.sleep(.fromSeconds(1), .awake) catch {};
 }
 ```
 The above code waits one second and gives both results, showing the importance of async/await. How fast your code will run depends on the I/O implementation that you choose, taking the advantage of the expressed asyncrony your code will be abel to go faster. Here we used std.Io.Threaded as our I/O implementation.

## What I Learned Today

- Async in Zig

## Looking Ahead

Tomorrow I will continue with exploring the article by [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html) to further explore async.

**References**

1. [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html)
