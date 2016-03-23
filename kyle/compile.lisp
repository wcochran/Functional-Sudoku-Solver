(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load "sudoku.asd")
(asdf:operate :build-op "sudoku")
(quit)
