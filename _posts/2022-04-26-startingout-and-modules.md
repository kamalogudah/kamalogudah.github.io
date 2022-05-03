---
title: 'Day 1: Starting Out and Modules'
categories:
  - Erlang
tags:
  - Programming 101
  - Modules
  - Immutability
  - Data Structures
---
With Erlang you can try out the language using the Erlang shell by typing  `erl`, one great thing about the Erlang shell is that it has a built-in line editor based on Emacs, allowing you to use something like `CTRL-A` and `CTRL-E` to move your cursor to the beginning and the end of a line.
  
Pressing `CTRL-G` and then `h` shows yet another power or Erlang shell, turns out the Erlang shell is a buncle of shell instances, each running different jobs you can type something like `j` to list all the jobs.

**Erlang Basics**
Erlang is a small language having few built-in types upon which a few more syntactic elements have been added. One thing unique about Erlang is that expressions have to be terminated with a period followed by whitespace (line break, a space etc.).

**1. Numbers**

  ```erlang
    1> 2 + 15.
    17
    2> 5 / 2.
    2.5
   ```
To get an integer-to-integer division, you have to use `div`, and to have the modulo operator, use `rem` (remainder), same story in Elixir.
```erlang
  1> 5 div 2.
  2
  2> 5 rem 2.
  1
 ```
**2. Invariable Variables**
Variables in Erlang are slightly different there is no assignment but pattern matching The first thing these commands tell us is that you can assign a value to a variable exactly once; then you can 'pretend' to assign a value to a variable if it's the same value it already has. If it's different, Erlang will complain.
```erlang
  1> One.
   * 1: variable 'One' is unbound
  2> One = 1.
   1
 ```
 Match operator in action.

 ```erlang
  1> 47 = 45 + 2.
  47
  2> 47 = 45 + 3.
   ** exception error: no match of right hand side value 48
 ```
**3. Atoms**
Atoms are literals, constants with their own name for value. They are synonymous to symbols in Ruby, they **are lower case** thats is why variables in Erlang are capitalized.
 ```erlang
  1> atom.
    atom
  2> atoms_rule.
    atoms_rule
  3> atoms_rule@erlang.
   atoms_rule@erlang
  4> 'Atoms can be cheated!'.
    'Atoms can be cheated!'
  5> atom = 'atom'.
   atom
 ```
**4. Boolean Algrebra and Comparison Operators**
Erlang allows for comparison of values using boolean, this is done as follows:

```erlang
  1> true and false.
    false
  2> false or true.
    true
  3> true xor false.
    true
  4> not false.
    true
  5> not (true and true).
    false
```
In Erlnag the boolean operators `and` and `or` will always evaluate arguments on both sides of the operator. If you want to have the short-circuit operators (which will only evaluate the right-side argument if it needs to), use `andalso` and `orelse`.
 
 To test for equality and inequality is where Erlang uses a different set of symbols compared to what you may be used to.
 ```erlang
   6> 5 =:= 5.
     true
   7> 1 =:= 0.
     false
   8> 1 =/= 0.
     true
   9> 5 =:= 5.0.
     false
   10> 5 == 5.0.
     true
   11> 5 /= 5.0.
     false
```
Instead of  `==` and `!=` that is common in many languages, Erlang uses `=:=` and `=/=` to test for equality and inequality respectively.
Another tripping point is `=< (less than or equal to)`. which is a little confusing if you may ask me.
```erlang
  12> 1 < 2.
    true
  13> 1 < 1.
    false
  14> 1 >= 1.
    true
  15> 1 =< 1.
   true
``` 
Erlang has no such things as boolean true and false. The terms true and false are atoms

**5. Tuples**
A tuple is a way to organize data. It groups together many terms when you know how many there are. In Erlang, a tuple is written in the form {Element1, Element2, ..., ElementN}.
```erlang
  1> X = 10, Y = 4.
   4
  2> Point = {X,Y}.
   {10,4}
```
 A tuple which contains an atom with one element following it is called a `tagged tuple`. Any element of a tuple can be of any type, even another tuple:
 ```erlang
   12> {point, {X,Y}}.
    {point,{4,5}}
 ```
**6. Lists**
Lists are a key component of functionalm programming, helping solve a multitude of problems, in Erlang they are among the most used data structures. You can put anything in a list from Numbers, atoms, tuples, other lists. The basic notation of a list is [Element1, Element2, ..., ElementN].
```erlang

  1> [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
    [1,2,3,{numbers,[4,5,6]},5.34,atom]
```
In Erlang Strings are lists:

```erlang
  1> [97, 98, 99].
    "abc"
  2> [233].
    "é"
```
Erlang will print lists of numbers as numbers only when at least one of them could not also represent a letter! There is no such thing as a real string in Erlang!
lists are joined using the `++` operator. The opposite of `++` is `--` which removes elements from a list:
The first element of a list is named the `Head`, and the rest of the list is named the `Tail`. We will use two built-in functions (BIF) to get them.
```erlang
  11> hd([1,2,3,4]).
  1
  12> tl([1,2,3,4]).
   [2,3,4]
```

**7. List Comprehensions**
List comprehensions allows for building or modifying lists. They also make programs short and easy to understand compared to other ways of manipulating lists. It's based off the idea of `set notation`.
```erlang
  1> [2*N || N <- [1,2,3,4]].
    [2,4,6,8]
```

```erlang
  2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
   [2,4,6,8,10]
```

```erlang
  3> RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}].
[{steak,5.99},
{beer,3.99},
{poutine,3.5},
{kitten,20.99},
{water,0.0}]
4> [{Item, Price*1.07} || {Item, Price} <- RestaurantMenu, Price >= 3, Price =< 10].
[{steak,6.409300000000001},{beer,4.2693},{poutine,3.745}]

```

```erlang
  6> Weather = [{toronto, rain}, {montreal, storms}, {london, fog},  
  6>            {paris, sun}, {boston, fog}, {vancouver, snow}].
  [{toronto,rain},
  {montreal,storms}, 
  {london,fog},
  {paris,sun},
  {boston,fog},
  {vancouver,snow}]
  7> FoggyPlaces = [X || {X, fog} <- Weather].
   [london,boston]

```
**7. Binary Data**

Erlang makes it easy to work with binary data using pattern matching to a great success.
```erlang
  1> Color = 16#F09A29.
    15768105
   2> Pixel = <<Color:24>>.
    <<240,154,41>>
```
This basically says "Put the binary values of #F09A29 on 24 bits of space (Red on 8 bits, Green on 8 bits and Blue also on 8 bits) in the variable Pixel." The value can later be taken to be written to a file.

Binary comprehensions are to bit syntax what list comprehensions are to lists: a way to make code short and concise. 
```erlang
  1> [ X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0].    
    [2,4]
```
The only change in syntax from regular list comprehensions is the <- which became <= and using binaries (<<>>) instead of lists ([]).
**Modules**
Modules are a bunch of functions regrouped in a single file, under a single name. Additionally, all functions in Erlang must be defined in modules.
Below is an example of how a module is written in Erlang.

```erlang
-module(what_the_if).
-export([heh_fine/0,oh_god/1, help_me/1]).

heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 1, 1 =:= 1 ->
        fail
    
    end.

% what_the_if:oh_god(2)

oh_god(N) ->
  if N =:= 2 -> might_succeed;
  true -> always_does %% This is Erlang's if's 'else!'
end.

% what_the_if:help_me(dog).
help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == beef -> "mooo";
              Animal == dog -> "bark";
              Animal == tree -> "bark";
              true -> "ghfghsafghdsgha"
            end,
    {Animal, "says " ++ Talk ++ "!"}.

```

**References**

  For more information on this topic you can checkout the following amazing posts:
1. [Starting out for Real](https://learnyousomeerlang.com/starting-out-for-real)
1. [Modules: What are modules](https://learnyousomeerlang.com/modules)



