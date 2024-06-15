Cobbling an Interpreter, June 24-28, 2024
=========================================

Our goal: Implement an interpreter in Rust from the first part of
[Crafting Interpreters](https://craftinginterpreters.com).

Some back-story: I've been thinking about the possibility of offering
a "Programming Languages" course for some time.  This course is a bit
of an experiment into how it might work--by trying to implement a simple
programming language using one of the most modern programming languages
around.   There should be more than enough to talk about.

Setup
-----

First, you should clone and make your own branch of the repo.

```
bash $ git clone https://github.com/dabeaz-course/rust_2024_06
bash $ cd rust_2024_06
bash $ git checkout -b yourname
bash $ git push -u origin yourname
```

For the remainder of the week, you should do all of the work in your
dedicated branch of the repository.  Moreover, I strongly encourage
you to commit and push the code in your branch throughout the
course. For example:

```
bash $ git commit -m "Solved some problem"
bash $ git push
```

I would encourage you to look at other branches, copy code, and make
comments.   I will be working from the "dabeaz" branch.

Real-time Chat
--------------

The course uses Zoom chat and Github discussions.  [Github discussions](https://github.com/dabeaz-course/rust_2024_06/discussions)
is a better option for topics involving code examples.

I would also encourage you to make a brief introduction on [here](https://github.com/dabeaz-course/rust_2024_06/discussions/1).

Zoom Meeting Link
-----------------

The live course is conducted via Zoom.  The official course hours are
09:30-16:30 US CDT/CHICAGO (UTC-05:00).  

Topic: Cobbling an Interpreter in Rust

Join Zoom Meeting
https://us02web.zoom.us/j/82818442054?pwd=Z2dubytDZlhTZEMraHNwMlpzOVRLZz09

Meeting ID: 828 1844 2054  
Passcode: 742919  

Resources
---------

* [Crafting Interpreters](https://craftinginterpreters.com)
* [Rust Programming Language](https://www.rust-lang.org)

A few tutorials recommended to me by a reputable Rust expert and former student.

* [Comprehensive Rust](https://google.github.io/comprehensive-rust/)
* [Rust by Example](https://doc.rust-lang.org/rust-by-example/)

Please add links to more resources if you know of something good!

Topics to Focus On
------------------

The focus of the week is on the somewhat narrow problem of
implementing the Lox interpreter from Crafting Interpreters.  As such,
it's definitely NOT necessary to know all of Rust.  However, the
following topics come to mind and might serve as a guide when
reviewing some Rust basics.

* Text and string manipulation.  We'll need to write a scanner and parser.

* Recursion.  Maybe of the algorithms and data structures used in
  implementing an interpreter involve recursion.

* Trees.  We'll need to represent an abstract syntax tree (AST)
  and perform various tasks while traversing the tree.

* Pattern matching.  We'll need to write a lot of code involving
  pattern matching over AST tree nodes.

* Environments. An interpreter typically involves a mutable runtime
  environment of variables (stack frames, etc.) that gets built
  using hash-maps or trees of some kind.
  
My gut sense is that everything will be a lot easier if we can focus
on using a more "functional" programming style based on functions and
immutable data structures.


