---
title: 'Day 16: Introduction to Zig Constants and Variables'
categories:
  - Zig
tags:
  - Programming 101
  - Zig
  - 1000DaysOfZig
  - BookReview
  - Introduction to 
  - constants
  - variables
---
## Constants and Variables
Zig supports constant and variable objects, which are immutable and mutable respectively. Mutable objects require explicit type annotations, unlike constant objects.
```zig
var age: u8 = 24;
```
## Undefined 
One can declare a variable without an initial value, though it is preferred to always initialize the objects you create. This involves using the special keyword `undefined`. Always declare and initialize your objects, do not rely on undefined unless there is a strong reason to do that.

## No unused objects
Zig does not allow for unused objects, every object declared in Zig has to be used. The object can be given to a function call, be an argument or it could be used in another expression to create other objects. You just have to make use of it, the Zig compiler will not compile when an object is not used, one will get an error in return.
By declaring a new object in Zig, one has two choices:
1. Use the value of the object.
2. Explicitly discard the value of the object.

Check example below how a discard is done:
```zig
// It compiles!
const age = 15;
_ = age;
```
## Variable objects must be mutated

When you declare a variable object in Zig as in the example below:
```zig
// It compiles!
var age = 15;
_ = age;
```
You have to mutate the object at some point during the program, failure to do so raises an error warning you about the issue.

## What I Learned Today

- Continued working on [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria

## Looking Ahead

Tomorrow I will continue with exploring [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria. 

**References**

1. [Introduction to Zig - a project-based book](https://pedropark99.github.io/zig-book/) By Pedro Duarte Faria
