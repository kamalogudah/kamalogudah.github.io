---
title: 'Day 2: Syntax in Functions'
categories:
  - Erlang
tags:
  - Programming 101
  - Pattern Matching
  - 60daysofErlang
---
  An `=` sign in Erlang/Elixir is not about assignment but rather is a match operator, the same way it is in Algebra. Pattern matching allows for using different function clauses to match different cases, this results in having methods with the same name and arity unlike in languages such as Ruby.
  
  ```erlang
    function(X) ->
      Expression;
    function(Y) ->
      Expression;
    function(_) ->
      Expression.

   ```
Above we have three *functional clauses* separated by semicolons(;) the three form a *function declaration* notice they have the same name and arity.

When a variable is bound, it has a value connected to it. When a variable is unbound, no value is connected to the variable. Once bound, the value of the variable cannot be changed. The variable is immutable in Erlang and Elixir.

**Guards**
In Erlang guards are used as additional clauses in function's head to make pattern matching more expressive.
Something like this: 

  ```erlang
    old_enough(0) -> false;
    old_enough(1) -> false;
    old_enough(2) -> false;
    ...
    old_enough(14) -> false;
    old_enough(15) -> false;
    old_enough(_) -> true.

   ```
Could be represented as:


  ```erlang
    old_enough(X)  when X >= 16 -> 
      true;
    old_enough(_) -> 
      false.

   ```
The above is not only shorter but clearer, compared to the former. Guards must return true to succeed, allowing to have multiple guard statements. If you have any number of comma separated guards all have to succeed for the entire guard to pass.

  ```erlang
    right_age(X)  when X >= 16, X =< 104 -> 
      true;
    right_age(_) -> 
      false.

   ```
Guards however do not accept user-defined functions due to side effects, Erlang is not a pure functional language like Haskell as it operates on some side effects.

**If Clauses**
In Erlang `if` clauses are like guards in their syntax but they are not in the function clause's head like guard clauses are.

A module showing use of `if` statement in Erlang:

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
Using `true` is common in Erlang however it is not the best thus instead of doing this

```erlang
   if X > Y -> a()
    : true -> b()
   end
 ```

 You can do something like:

 ```erlang
   if X > Y -> a()
    : X =< Y  -> b()
   end
 ```

 **Case expressions**
 Case expressions is like the whole function head, the same way we compare an `if` to a guard.

 ```erlang
   -module(cases).
   -export([insert/2, beach/1]).

   insert(X,[]) ->
     [X];
   insert(X,Set) ->
    case lists:member(X,Set) of
      true -> Set;
      false -> [X|Set]
    end.

   beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favourable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favourable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favourable in the us';
        _ ->
            'avoid beach'
    end.

 ```
 The above can be represented as:
  ```erlang
    beachf({celsius, N}) when N >= 20, N =< 45 ->
       'favourable';
    ...
    beachf(_) ->
      'avoid beach'.
  ```

**References**
For more information on this topic you can checkout the following amazing posts:
1. [Syntax in Functions](https://learnyousomeerlang.com/syntax-in-functions)


