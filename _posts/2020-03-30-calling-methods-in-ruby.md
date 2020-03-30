---
title: 'Different ways to call a method in Ruby'
categories:
  - Ruby
tags:
  - hacks
---

Ruby has many ways of doing the same thing, for instance when you have an array to get itys size you could either use one of the following methods: Array.size, Array.length, Array.size or Array.count.
When it comes to calling methods on objects in Ruby one has a number of alternatives to choose from, some are just fancy ways to use in the commdand line and should not be used in actual code.

1. Method One.

```
 str = "Calling Methods"
 str.upcase # CALLING METHODS

```

2. Method Two.

```
 str = "Calling Methods"
 str.send(:upcase) # CALLING METHODS

```

2. Method Three.

```
 str = "Calling Methods"
 m = str.method(:upcase)
 m.call # CALLING METHODS

```

And that is it for today.
