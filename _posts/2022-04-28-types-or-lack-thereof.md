---
title: 'Day 3: Types (Or Lack Thereof) and Recurssion'
categories:
  - Erlang
tags:
  - Type System
  - 60daysofErlang
---
  Type Safety is the new cool, type systems give programmers more safety and speed due to less errors. Erlang is a dynamically typed language where error is caught at runtime and not compile time.
  Many developers are of the opinion that statically typed languages have more safety compared to their dynamically typed counterparts. This may be true to when comparing with most dynamically typed languages but not Erlang. Erlang accounts for most errors through strategies such as distribution over different nodes and handling unexpected errors.
  
  Erlang can be considered to be strongly typed since it hardly does implicit type conversions between terms.
  ```erlang
  1> 6 + "1".
    ** exception error: bad argument in an arithmetic expression
     in operator  +/2
      called as 6 + "1"
  ```

**Type Conversion**
Erlang has a number of methods implemented as BIFs which help with type conversion, 
```erlang
  1> erlang:list_to_integer("54").
   54
  2> erlang:integer_to_list(54).
   "54"
```

**Guards**
Erlang has functions used as guiard clauses to help ensure that data of patterns match a given type, these includes among others the following:

```erlang
is_atom/1           is_binary/1        
is_bitstring/1      is_boolean/1        is_builtin/3       
is_float/1          is_function/1       is_function/2      
is_integer/1        is_list/1           is_number/1        
is_pid/1            is_port/1           is_record/2        
is_record/3         is_reference/1      is_tuple/1     
```
In terms of data structures Erlang has a limited set, however lists and tuples are usually enough to build other complex structures without worrying about anything.
  
**Recursion**
Recursion is the main work horse for creating loops in functional programming, an example of recursion is doing factorial which in Erlang can be done like below:
```erlang
  -module(recursive).
  -export([fac/1]).
 
  %fac(N) when N == 0 -> 1;
  %fac(N) when N > 0  -> N*fac(N-1).

  fac(0) -> 1;
  fac(N) when N > 0 -> N*fac(N-1).

  len([]) -> 0;
  len([_|T]) -> 1 + len(T).
```
Recursion can be summarized as a function that calls itself. To do recursion one needs to have a stopping condition (`base case`), this helps prevent infinite looping. 
**Tail Recursion**
Tail recursion is a way to transform the linear processing of lists (it grows as much as there are elements) to an iterative one (there is not really any growth).
The answer to 1 + len(Rest) needs the answer of len(Rest) to be found. The function len(Rest) itself then needed the result of another function call to be found. The additions would get stacked until the last one is found, and only then would the final result be calculated. Tail recursion aims to eliminate this stacking of operation by reducing them as they happen.

In order to achieve this, we will need to hold an extra temporary variable as a parameter in our function. I'll illustrate the concept with the help of the factorial function, but this time defining it to be tail recursive. The aforementioned temporary variable is sometimes called `accumulator` and acts as a place to store the results of our computations as they happen in order to limit the growth of our calls:
```erlang
tail_fac(N) -> tail_fac(N,1).
 
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).
```

Tail recursive length function.
```erlang
tail_len(L) -> tail_len(L,0).
 
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).
```

**References**

  For more information on this topic you can checkout the following amazing posts:
1. [Types (or lack thereof)](https://learnyousomeerlang.com/types-or-lack-thereof)
2. [Recursion](https://learnyousomeerlang.com/recursion)

