---
title: 'Day 7: A Short Visit to Common Data Structures'
categories:
  - Data Structures
tags:
  - Common Data Structures
  - 60daysofErlang
  
---
**Records**
Records are, first of all, a hack. They are more or less an afterthought to the language and can have their share of inconveniences.They're still pretty useful whenever you have a small data structure where you want to access the attributes by name directly. As such, Erlang records are a lot like structs in C (if you know C.). Here is how they are declared.
```erlang
-module(records).
-compile(export_all).
 
-record(robot, {name,
type=industrial,
hobbies,
details=[]}).
```
And below is how it is used in the code.
```erlang
first_robot() ->
  #robot{name="Mechatron",
     type=handmade,
     details=["Moved by a small man inside"]}.
```
Running the code you the following:
```erlang
  1> c(records).
    {ok,records}
  2> records:first_robot().
   {robot,"Mechatron",handmade,undefined,
    ["Moved by a small man inside"]}
```
Erlang records are just syntactic sugar on top of tuples. The Erlang shell has a command `rr(Module)` that lets you load record definitions from Module.
```erlang
  3> rr(records).
  [robot]
  4> records:first_robot().        
   #robot{name = "Mechatron",type = handmade,
     hobbies = undefined,
    details = ["Moved by a small man inside"]}
```
You can extract values from records through using a dot syntax:

```erlang
  5> Crusher = #robot{name="Crusher", hobbies=["Crushing people","petting cats"]}.
    #robot{name = "Crusher",type = industrial,
      hobbies = ["Crushing people","petting cats"],
      details = []}
  6> Crusher#robot.hobbies.
    ["Crushing people","petting cats"]
```

**Key-Value Stores**
For small amounts of data, there are basically two data structures that can be used. The first one is called a `proplist`. A proplist is any list of tuples of the form [{Key,Value}]. To work with proplists, you can use the proplists module. It contains functions such as `proplists:delete/2`, `proplists:get_value/2`, `proplists:get_all_values/2`, `proplists:lookup/2` and `proplists:lookup_all/2`.
If you do want a more complete key-value store for small amounts of data, the orddict module is what you need. `Orddicts` (ordered dictionaries) are proplists with a taste for formality. Each key can be there once, the whole list is sorted for faster average lookup, etc. Common functions for the CRUD usage include `orddict:store/3`, `orddict:find/2`

There are basically two key-value structures/modules to deal with larger amounts of data: `dicts` and `gb_trees`. Dictionaries have the same interface as orddicts: dict:store/3, dict:find/2, dict:fetch/2, dict:erase/2 and every other function, such as dict:map/2 and dict:fold/2 (pretty useful to work on the whole data structure!) Dicts are thus very good choices to scale orddicts up whenever it is needed.

**Arrays**
Arrays allow you to access elements with numerical indices and to fold over the whole structure while possibly ignoring undefined slots.

**A Set of Sets**
Sets are groups of unique elements that you can compare and operate on: find which elements are in two groups, in none of them, only in one or the other, etc. To work with sets in Erlang there are four modules, that is `ordsets`, `sets`, `gb_sets` and `sofs (sets of sets)`. Despite a variaety of ways of using sets it is recommended that one should use `gb_sets` in most circumstances, using `ordset` when you need a clear representation that you want to process with your own code and `sets` when you need the =:= operator.

**Directed Graphs**
Directed graphs in Erlang are implemented as two modules, digraph and digraph_utils. The digraph module basically allows the construction and modification of a directed graph: manipulating edges and vertices, finding paths and cycles, etc. On the other hand, digraph_utils allows you to navigate a graph (postorder, preorder), testing for cycles, arborescences or trees, finding neighbors, and so on.

Because directed graphs are closely related to set theory, the 'sofs' module contains a few functions letting you convert families to digraphs and digraphs to families.

**Queues**
The `queue module`implements a double-ended `FIFO (First In, First Out)` queue. They're implemented a bit as two lists (in this context, stacks) that allow to both append and prepend elements rapidly.The queue module basically has different functions in a mental separation into 3 interfaces (or APIs) of varying complexity, called `Original API`, `Extended API` and `Okasaki API`.You'll generally want to use queues when you'll need to ensure that the first item ordered is indeed the first one processed.

**References**

  For more information on this topic you can checkout the following amazing posts:
1. [A Short Visit to Common Data Structures](https://learnyousomeerlang.com/a-short-visit-to-common-data-structures)


