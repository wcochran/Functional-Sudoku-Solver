(in-package #:sudoku)

(deftype rc-index () '(integer 0 9))
(deftype sudoku-grid () '(simple-array fixnum (81)))

(defun grid-elem (grid row col)
  (declare (type rc-index row col)
           (type sudoku-grid grid))
  "Fetch Sudoku grid value at (row,col)."
  (aref grid (+ (* row 9) col)) )  ; row-major index

(defun grid-ref (grid index)
  (declare (type fixnum index)
           (type sudoku-grid grid))
  (aref grid index))

(defun update-grid (grid row col num)
  (declare (type rc-index row col)
           (type fixnum num)
           (type sudoku-grid grid))
  "Copy input grid into new grid is updated with 'num' at (row,col)."
  (let ((new-grid (copy-seq grid)))
    (setf (aref new-grid (+ (* row 9) col)) num)
    new-grid) )

(defun puzzle-from-string (string)
  (map 'sudoku-grid
       (lambda (c)
   (cond
    ((char= c #\.) 0)
    (t (- (char-code c) (char-code #\0))) ) )
       string) )

(defun grid-find-helper (grid value index)
  (declare (type fixnum value index)
           (type sudoku-grid grid))
  (cond
    ((>= index 81) nil)
    ((= (grid-ref grid index) value) index)
    (t (grid-find-helper grid value (+ index 1))) ))

(defun grid-find (grid value)
  (declare (type sudoku-grid grid)
           (type fixnum value))
  (grid-find-helper grid value 0))
