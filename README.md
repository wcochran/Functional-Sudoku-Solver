Soduku Solver using Backtracking
================================

Author: W. Cochran ([wcochran@wsu.edu](mailto://wcochran@wsu.edu))

Description
-----------

This is a simple Sudoku Solver using Bactracking using
a *purely functional style* of programming. Currently there
is a Common Lisp and JS version of the solver that has
the following features:

* *Immutable Sudoku Grid*:  Each grid vector is treated as being immutable.
     When we need to update a grid with a new value, we make a copy of the
     grid and mutate the copy. Creates a lot of garbage (may want to turn
     of GC messages). Can be easily changed to simply mutate a single grid
     which would drastically improve memory performance.

* *No iteration used*: Tail recursion frequently used which can be
     compiled into efficient iteration code.

* *No side effects*: each function computes a new value and modifies
     nothing else in the environment.


Common Lisp
-----------
Using CMU Lisp at the top-level you can compile, load, and run using the test
puzzle as follows (since there is a lot of GC activity you will want to turn
off verbose mode):

     * (compile-file "Sudoku.lisp") ; builds a sse2f file on x86-64 box
     * (load "Sudoku.sse2f")        ; load compiled file
     * (setf extensions:*gc-verbose* nil)  ; ignore GC warnings
     * (solve *puzzle*)             ; solve example puzzle
     
In the kyle/ subdirectory there is Kyle Siehl's version of the code that uses
type annotations so that the compiler can statically type the grid. Using Steel Bank Common Lisp (with QuickLisp package manager) he was able to create a extremely fast version.
     
JavaScript
----------

I used Node.js for the JS version. `Sudoku.js` uses the `new SudokuPuzzle` to create objects whereas `Sudoku2.js` uses `Object.create()`. 
See [SO discussion](http://stackoverflow.com/questions/36183602/why-is-nodes-object-createfoo-much-slower-then-new-foo") on how to use `Object.create` correctly.
To try with example puzzle simply do the following:

     node Sudoku.js
     node Sudoku2.js
