Soduku Solver using Backtracking
================================

Author W. Cochran wcochran@wsu.edu

This is a simple Sudoku Solver using Bactracking using
a purely functional style of programming. Currently there
is a Common Lisp and JS version of the solver that have
the following features:

* Immutable Sudoku Grid:  Each grid vector is treated as being immutable. 
     When we need to update a grid with a new value, we make a copy of the 
     grid and mutate the copy. Creates a lot of garbage (may want to turn
     of GC messages). Can be easily changed to simply mutate a single grid
     which would drastically improve memory performance.

* No iteration used: Tail recursion frequently used which can be
     compiled into efficient iteration code.

* No side effects: each function computes a new value and modifies
     nothing else in the environment.

