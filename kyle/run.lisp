(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load "sudoku.asd")
(require :sudoku)
(sudoku:test)
(quit)
