---
title: 'Day 5: Errors and Exceptions'
categories:
  - Erlang
tags:
  - Errors
  - 60daysofErlang
  
---
Errors come in many flavours such as compile-time errors, logical errors, run-time errors and generated errors. Compile-time errors are often syntactic mistakes: check your function names, the tokens in the language (brackets, parentheses, periods, commas), the arity of your functions, etc.

Logical errors are the hardest kind of errors to find and debug. They're most likely errors coming from the programmer: branches of conditional statements such as 'if's and 'case's that don't consider all the cases, mixing up a multiplication for a division, etc. They result in bad data.

Run-time errors are pretty destructive in the sense that they crash your code. While Erlang has ways to deal with them, recognizing these errors is always helpful. These include among others `function_clause`, `case_clause` and `if_clause`.

**Raising Exceptions**
There are three kinds of exceptions in Erlang: `errors`, `throws` and `exits`. They all have different uses (kind of):

**Errors**
Calling `erlang:error(Reason)` will end the execution in the current process and include a stack trace of the last functions called with their arguments when you catch it. These are the kind of exceptions that provoke the run-time errors.

```erlang
1> erlang:error(badarith).
** exception error: bad argument in an arithmetic expression
2> erlang:error(custom_error).
** exception error: custom_error
```

**Exits**
There are two kinds of exits: `internal` exits and `external` exits. `Internal exits` are triggered by calling the function `exit/1` and make the current process stop its execution. `External exits` are called with `exit/2` and have to do with multiple processes in the concurrent aspect of Erlang.

**Throws**
A throw is a class of exceptions used for cases that the programmer can be expected to handle. In comparison with exits and errors, they don't really carry any `crash that process!` intent behind them, but rather control flow. As you use throws while expecting the programmer to handle them, it's usually a good idea to document their use within a module using them.
```erlang
1> throw(permission_denied).
** exception throw: permission_denied
```

**Dealing with Exceptions**
Exceptions are handled using a `try ... catch` expression. It's syntax is as follows:

```erlang
try Expression of
  SuccessfulPattern1 [Guards] ->
    Expression1;
  SuccessfulPattern2 [Guards] ->
    Expression2
catch
  TypeOfError:ExceptionPattern1 ->
    Expression3;
  TypeOfError:ExceptionPattern2 ->
    Expression4
end.
```

```erlang
  -module(exceptions).
  -compile(export_all).
 
  throws(F) ->
    try F() of
      _ -> ok
    catch
      Throw -> {throw, caught, Throw}
    end.
  errors(F) ->
    try F() of
      _ -> ok
    catch
      error:Error -> {error, caught, Error}
    end.
 
  exits(F) ->
    try F() of
      _ -> ok
    catch
      exit:Exit -> {exit, caught, Exit}
    end.
  ```

```erlang
1> c(exceptions).
{ok,exceptions}
2> exceptions:throws(fun() -> throw(thrown) end).
{throw,caught,thrown}
3> exceptions:throws(fun() -> erlang:error(pang) end).
** exception error: pang
5> exceptions:errors(fun() -> erlang:error("Die!") end).
{error,caught,"Die!"}
6> exceptions:exits(fun() -> exit(goodbye) end).
{exit,caught,goodbye}
```

**References**

  For more information on this topic you can checkout the following amazing posts:
1. [Errors and Exceptions](https://learnyousomeerlang.com/errors-and-exceptions)


