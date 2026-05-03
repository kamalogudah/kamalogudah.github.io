---
title: 'Day 14: Async in Zig '
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - async
---

The 14th day of my 1000 days of Zig was spent on going further into Async in Zig using the article by [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html). In the previous post we looked at handling failure and using cancelation when using Async. 

## Resource Allocation
How do we handle resource allocation, to demonstrate this we allocate a string on success.
```zig
fn juicyMain(gpa: Allocator, io: Io) !void {
    var a = io.async(doWork, .{ gpa, io, "hard" });
    defer if (a.cancel(io)) |s| gpa.free(s) else |_| {};

    var b = io.async(doWork, .{ gpa, io, "on an excuse not to drink Spezi" });
    defer if (b.cancel(io)) |s| gpa.free(s) else |_| {};

    const a_string = try a.await(io);
    const b_string = try b.await(io);
    std.debug.print("finished {s}\n", .{a_string});
    std.debug.print("finished {s}\n", .{b_string});
}

fn doWork(gpa: Allocator, io: Io, flavor_text: []const u8) ![]u8 {
    const copied_string = try gpa.dupe(u8, flavor_text);
    std.debug.print("working {s}\n", .{copied_string});
    io.sleep(.fromSeconds(1), .awake) catch {};
    return copied_string;
}
```

Looking at the above we understand why cancel and await have the same API. The deferred cancel calls above free the allocated resource, handling both successful calls (resource allocated) and failed calls (resource not allocated). Using the above approach to resource management allows a developer to write idiomatic code, using try and return normally without any concern about cases that require special resource management.

## Asynchrony is not concurrency
Asynchrony does not necessarily mean concurrency; for more on this topic, check out [Loris Cro's article](https://kristoff.it/blog/asynchrony-is-not-concurrency/). Building on our example in this series we have the code below, in this example we have a producer sending one item across an unbuffered queue to a consumer.

```zig
fn juicyMain(io: Io) !void {
    var queue: Io.Queue([]const u8) = .init(&.{});

    var producer_task = io.async(producer, .{
        io, &queue, "never gonna give you up",
    });
    defer producer_task.cancel(io) catch {};

    var consumer_task = io.async(consumer, .{ io, &queue });
    defer _ = consumer_task.cancel(io) catch {};

    const result = try consumer_task.await(io);
    std.debug.print("message received: {s}\n", .{result});
}

fn producer(
    io: Io,
    queue: *Io.Queue([]const u8),
    flavor_text: []const u8,
) !void {
    try queue.putOne(io, flavor_text);
}

fn consumer(
    io: Io,
    queue: *Io.Queue([]const u8),
) ![]const u8 {
    return queue.getOne(io);
}
```

Running this we get the following output:
```zig
0s $ zig run example8.zig
0s message received: never gonna give you up
0s $
```
## What I Learned Today

- Resource allocation when using Async in Zig.
- Asynchrony is not concurrency.

## Looking Ahead

Tomorrow I will continue exploring the article by [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html) to further understand async.

**References**

1. [Andrew Kelly](https://andrewkelley.me/post/zig-new-async-io-text-version.html)
2. [Loris Cro](https://kristoff.it/blog/asynchrony-is-not-concurrency/)
