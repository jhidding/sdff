# Notes on Software Design for Flexibility

The book uses the Scheme programming language to explain concepts. I'll translate what I can to Python to make examples a bit more accessible. Most things will still be primarily Scheme though. I recommend installing [Chez Scheme](https://cisco.github.io/ChezScheme/), which is a very good implementation of a Scheme compiler. At the same time let's try to stick to the R6RS dialect, for which [The Scheme Programming Language](https://www.scheme.com/tspl4/) is a good reference.

## Non-standard syntax
The book uses some non-standard syntax. We can do two things: minimize the amount of SRFIs needed by using standard Scheme as much as we can, and add SRFI's or other syntax until everything just works:

- SRFI 219: short-hand higher order function syntax.
