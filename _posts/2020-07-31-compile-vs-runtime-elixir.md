---
title: 'Compile vs Runtime Evaluation in Elixir'
categories:
  - Elixir
tags:
  - Productivity
---
Elixir despite being a compiled language, it somehow behaves like an interpreted language. For instance
Usually you would not put an `Application.get_env` or `System.get_env` call as a module attribute since it is evaluated at compile time and usually you are looking to have these things configured at run-time.

Compile vs Runtime evaluation can results in issues in different production environments,
at times what you really want is for example an environment variable to be evaluated at runtime, instead of at compile-time or the other way around.

  For more information on this topic you can checkout the following amazing posts:
1. [Configuring your Elixir Application at Runtime with Vapor](https://blog.appsignal.com/2020/04/28/configuring-your-elixir-application-at-runtime-with-vapor.html)
2. [Elixir: Runtime vs. compile time configuration](https://www.amberbit.com/blog/2018/9/27/elixir-runtime-vs-compile-time-configuration/)
3. [Understanding Compile Time Dependencies in Elixir - A Bug Hunt](https://stephenbussey.com/2019/01/03/understanding-compile-time-dependencies-in-elixir-a-bug-hunt.html)
4. [Speeding up re-compilation of Elixir projects](https://dashbit.co/blog/speeding-up-re-compilation-of-elixir-projects)






