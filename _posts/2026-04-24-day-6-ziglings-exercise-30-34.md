---
title: 'Day 6: Ziglings exercises 030 - 034'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - Ziglings
  - 1000DaysOfZig
---

On the 6th day of our 1000-day journey with Zig, most of the time was spent working on Ziglings. Presently we are at almost the 30th exercise mark, with a few more dozen to go. So far enjoying the learning and totally loving it.

## Switch 
Yesterday we looked at defer, errdefer among other things, today we get back to control flow selection using switch statements. With switch statements one can match possible values of an expression, thereby performing different actions based on each value.
```zig
     switch (players) {
         1 => startOnePlayerGame(),
         2 => startTwoPlayerGame(),
         else => {
             alert();
             return GameError.TooManyPlayers;
         }
     }
```
 
 This is similar to having this:
 ```zig
    if (players == 1) startOnePlayerGame();
  else if (players == 2) startTwoPlayerGame();
     else {
         alert();
         return GameError.TooManyPlayers;
     }
```
## Switch as expressions
Like if statements, one can use switch statements as expressions which return values that you can later use in your program.
```zig
     const a = switch (x) {
         1 => 9,
         2 => 16,
         3 => 7,
         ...
     }
```

## Unreachable Statement
Something unique to Zig is the unreachable statement, which is used when you want to tell the compiler that a given part of the code should not be executed, where reaching that part of the code is deemed an error.
```zig
if (true) {
         ...
     } else {
         unreachable;
     }
```
## If error
We have looked at a number of ways of handling errors such as using catch and try; another variation is using the if statement.
```zig
     if (foo) |value| {
         // foo was NOT an error; value is the non-error value of foo
     } else |err| {
         // foo WAS an error; err is the error value of foo
     }
```
You can resort to using a switch statement in this scenario, as shown below:
```zig
   if (foo) |value| {
         ...
     } else |err| switch (err) {
         ...
     }
```
This is similar but a different variation of the above statement.
## What I Learned Today

- Switch statement in Zig
- Unreachable statements in Zig.
- using if error statements.

## Looking Ahead

Tomorrow, is Saturday, I may take a short break but will find a way to doing something in Zig, you will see what I will write about in my 7 days of Zig.

**References**

1. [Ziglings](https://codeberg.org/ziglings)
