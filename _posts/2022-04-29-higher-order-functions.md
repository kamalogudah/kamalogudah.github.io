---
title: 'Day 4: Higher Order Functions'
categories:
  - Erlang
tags:
  - Higher Order Functions
  - Closures
  - 60daysofErlang
---
  In functional programming concept of higher order functions is where we use functions as parameters to other functions. Resulting in the function parameter being used as any other variable within the function. The origin of higher order functions is [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

  ```erlang
    -module(hhfuns).
    -compile(export_all).
  
    one() -> 1.
    two() -> 2.
 
    add(X,Y) -> X() + Y().
    increment([]) -> [];
    increment([H|T]) -> [H+1|increment(T)].
    decrement([]) -> [];
    decrement([H|T]) -> [H-1|decrement(T)].
    map(_, []) -> [];
    map(F, [H|T]) -> [F(H)|map(F,T)].
    incr(X) -> X + 1.
    decr(X) -> X - 1.

```
This results into:

```erlang
  1> c(hhfuns).
  {ok, hhfuns}
  2> hhfuns:add(one,two).
  ** exception error: bad function one
  in function  hhfuns:add/2
  3> hhfuns:add(1,2).
  ** exception error: bad function 1
  in function  hhfuns:add/2
  4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
  3
```

```erlang
  1> c(hhfuns).
  {ok, hhfuns}
  2> L = [1,2,3,4,5].
  [1,2,3,4,5]
  3> hhfuns:increment(L).
  [2,3,4,5,6]
  4> hhfuns:decrement(L).
  [0,1,2,3,4]
  5> hhfuns:map(fun hhfuns:incr/1, L).
  [2,3,4,5,6]
  6> hhfuns:map(fun hhfuns:decr/1, L).
  [0,1,2,3,4]
```

**Anonymous Functions**
Anonymous functions, or funs, address that problem by letting you declare a special kind of function inline, without naming them. They can do pretty much everything normal functions can do, except calling themselves recursively (how could they do it if they are anonymous?).

```erlang
  fun(Args1) ->
    Expression1, Exp2, ..., ExpN;
   (Args2) ->
     Expression1, Exp2, ..., ExpN;
   (Args3) ->
     Expression1, Exp2, ..., ExpN
  end
```
Anonymous functions can be used as below:
```erlang
  7> Fn = fun() -> a end.
  #Fun<erl_eval.20.67289768>
  8> Fn().
  a
  9> hhfuns:map(fun(X) -> X + 1 end, L).
  [2,3,4,5,6]
  10> hhfuns:map(fun(X) -> X - 1 end, L).
  [0,1,2,3,4]
```
More on anonymous functions:

```erlang
  11> PrepareAlarm = fun(Room) ->
  11>                     io:format("Alarm set in ~s.~n",[Room]),
  11>                     fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
  11>                   end.
  #Fun<erl_eval.20.67289768>
  12> AlarmReady = PrepareAlarm("bathroom").
  Alarm set in bathroom.
  #Fun<erl_eval.6.13229925>
  13> AlarmReady().
  Alarm tripped in bathroom! Call Batman!
  ok
```
Closure allows for functions to access variables within their scopes.
**References**

  For more information on this topic you can checkout the following amazing posts:
1. [Higher Order Functions](https://learnyousomeerlang.com/higher-order-functions#get-functional)


