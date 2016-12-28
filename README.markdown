Advent of Code 2016 in Common Lisp
==================================

My (Lucian Mogoșanu's) solutions for [Advent of Code 2016][1]. Share at
leisure, use at your own risk.

Notes on implementation
-----------------------

These solutions are a classic example of "train of thought" literate-ish
programming and at the same time they make up an engineering mess. Some
of the flaws which I'd like to fix in the future include:

* I've learned to program in Lisp along the way (one never ceases to
  learn Lisp), so some algorithms would benefit from standard Common
  Lisp data structures which are more convenient to use or that provide
  better performance. Some code would also benefit from the usage of
  idioms such as macros where they are better suited, or `with-gensyms`
  instead of `let ( ... (var (gensym)) ...)`.
* The code often suffers of sheer rawness (Day 8) or overengineering
  (most days in which I generate data structures using the parser
  instead of parsing + solving directly). Sometimes extra bits of
  engineering may be useless, other times it may be sorely needed. I
  need to think this through better.
* Code reuse through copy-paste sucks. While I'm not a big fan of
  importing libraries (which is why I tried to reduce their use to a
  bare minimum), some functionality, e.g. the exploration functions
  could be easily refactored into a separate library file.
* More refactoring, problem/algorithm rethinking, etc.

Having said that, I am fond of some of these solutions: for example the
cellular automaton in Day 18 and the MD5 key finder in Day 14 use a very
rudimentary implementation of (lazy, in Day 14) streams, which, however
rudimentary, fit their purpose wonderfully and are better than any
third-party library that I could have used.

As a side note, I learned using Emacs' Paredit mode while here, and now
it seems awkward to write code without it.

[1]: http://adventofcode.com/2016
