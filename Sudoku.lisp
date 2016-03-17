;;;;
;;;; Soduku Solver for Common Lisp using Backtracking
;;;; Author: W. Cochran  wcochran@wsu.edu
;;;;
;;;; Description:
;;;;    Purely functional Sudoku Solver using simple backtracking
;;;;    algorithm. A 9x9 Sudoku grid is represented by a 81 element vector
;;;;    of integers in row-major order. A value of 0 represents an unknown value;
;;;;    A value in the range 1..9 represents a (possible tentative) solved value.
;;;;
;;;;    Top-level function is 'solve' which takes a Sudoku grid vector and returns
;;;;    a new solved grid (or nil if there is no solution). Example usage:
;;;;       (setf *solved-puzzle* (solve *puzzle*))
;;;;   
;;;;    * Immutable Sudoku Grid:  Each grid vector is treated as being immutable. 
;;;;      When we need to update a grid with a new value, we make a copy of the 
;;;;      grid and mutate the copy. Creates a lot of garbage (may want to turn
;;;;      of GC messages). Can be easily changed to simply mutate a single grid
;;;;      which would drastically improve memory performance.
;;;;    * No iteration used: Tail recursion frequently used which can be
;;;;      compiled into efficient iteration code.
;;;;    * No side effects: each function computes a new value and modifies
;;;;      nothing else in the environment.
;;;;

;; . 9 4 | . . . | 1 3 .
;; . . . | . . . | . . .
;; . . . | . 7 6 | . . 2
;; ------+-------+------
;; . 8 . | . 1 . | . . .
;; . 3 2 | . . . | . . .
;; . . . | 2 . . | . 6 .
;; ------+-------+------
;; . . . | . 5 . | 4 . .
;; . . . | . . 8 | . . 7
;; . . 6 | 3 . 4 | . . 8
(defvar *puzzle-str* 
  ".94...13..............76..2.8..1.....32.........2...6.....5.4.......8..7..63.4..8")

(defun puzzle-from-string (string)
  (map 'vector
       (lambda (c)
	 (cond 
	  ((char= c #\.) 0)
	  (t (- (char-code c) (char-code #\0))) ) )
       string) )

(defvar *puzzle* (puzzle-from-string *puzzle-str*))

(defvar *solved-puzzle*
  #(7 9 4 5 8 2 1 3 6 
    2 6 8 9 3 1 7 4 5 
    3 1 5 4 7 6 9 8 2 
    6 8 9 7 1 5 3 2 4 
    4 3 2 8 6 9 5 7 1 
    1 5 7 2 4 3 8 6 9 
    8 2 1 6 5 7 4 9 3 
    9 4 3 1 2 8 6 5 7 
    5 7 6 3 9 4 2 1 8) )

(defun grid-elem (grid row col)
  "Fetch Sudoku grid value at (row,col)."
  (aref grid (+ (* row 9) col)) )  ; row-major index

(defun update-grid (grid row col num)
  "Copy input grid into new grid is updated with 'num' at (row,col)."
  (let ((new-grid (copy-seq grid)))
    (setf (aref new-grid (+ (* row 9) col)) num)
    new-grid) )

(defun conflicting-number-in-row (grid num row &optional (col 0))
  "Are there any values equal to 'num' in the given row?"
  (cond
   ((>= col 9) nil)
   ((= (grid-elem grid row col) num) t)
   (t (conflicting-number-in-row grid num row (+ col 1))) ) )

(defun conflicting-number-in-col (grid num col &optional (row 0))
  "Are there any values equal to 'num' in the given column?"
  (cond
   ((>= row 9) nil)
   ((= (grid-elem grid row col) num) t)
   (t (conflicting-number-in-col grid num col (+ row 1))) ) )

(defun conflicting-number-in-block (grid num row col &optional (r 0) (c 0))
  "Are there any values equal to 'num' in the 3x3 block with origin (row,col)?"
  (cond
   ((>= r 3) nil)
   ((= (grid-elem grid (+ row r) (+ col c)) num) t)
   ((< c 2)
    (conflicting-number-in-block grid num row col r (+ c 1)) )
   (t
    (conflicting-number-in-block grid num row col (+ r 1) 0) ) ) )

(defun conflicting-number (grid num row col)
  "Are there any values equal to 'num' in the row, column, or 3x3 block?"
  (or
   (conflicting-number-in-row grid num row)
   (conflicting-number-in-col grid num col)
   (conflicting-number-in-block grid num 
 				(* (truncate (/ row 3)) 3)
 				(* (truncate (/ col 3)) 3) ) ) )
    
(defun unassigned-location (grid &optional (index 0))
  "Find the index of first 0 in the grid (no zeros => return nil)."
  (cond
   ((>= index 81) nil)
   ((= (aref grid index) 0) index)
   (t (unassigned-location grid (+ index 1))) ) )

(defun solve (grid)
  "Top level solver : returns solved grid (or nil if no solution)."
  (let ((index (unassigned-location grid)))
    (cond
     ((null index) grid) ;; no zeroes then solution found!
     (t (let ((row (truncate (/ index 9)))
	      (col (mod index 9)) )
	  (let ((soln (solve-starting-at-cell grid row col)))
	    (cond
	     (soln soln)  ;; solution found!
	     (t nil) ) ) )) ) ) ) ;; no solution found (backtrack or fail)

(defun solve-starting-at-cell (grid row col &optional (num 1))
  "Attempts to solve grid using all digits 1..9 at (row,col)."
  (cond
   ((> num 9) nil) ;; tried all digits 1..9, no solution found
   ((conflicting-number grid num row col)  ;; can't put 'num' at (row,col)
    (solve-starting-at-cell grid row col (+ num 1)) ) ;; try next number
   (t (let ((soln (solve (update-grid grid row col num)))) ;; solve w new grid
	(cond
	 (soln soln)  ;; solution found!
	 (t (solve-starting-at-cell ;; else try next number
	     grid row col (+ num 1) )) ) )) ) )

;; (defvar *solved-puzzle* (solve *puzzle*))

