(in-package #:sudoku)

(deftype rc-index () '(integer 0 9))

(defstruct (bst (:conc-name bst-)
                (:constructor bst (key value left right)))
  (key 0 :type fixnum)
  (value 0 :type fixnum)
  left
  right)

(defun bst-lookup (bst key)
  (declare (type fixnum key))
  (cond ((= key (bst-key bst))
         (bst-value bst))
        ((< key (bst-key bst))
         (bst-lookup (bst-left bst) key))
        (t
         (bst-lookup (bst-right bst) key))))

(defun bst-update (bst key new-value)
  (declare (type fixnum key new-value))
  (cond ((= key (bst-key bst))
         (bst (bst-key bst)
               new-value
               (bst-left bst)
               (bst-right bst)))
        ((< key (bst-key bst))
         (bst (bst-key bst)
               (bst-value bst)
               (bst-update (bst-left bst)
                            key new-value)
               (bst-right bst)))
        (t
         (bst (bst-key bst)
               (bst-value bst)
               (bst-left bst)
               (bst-update (bst-right bst)
                            key new-value)))))

(defun grid-index (row col)
  (declare (type rc-index row col))
  (+ col (* 9 row)))

(defun grid-elem (grid row col)
  (declare (type rc-index row col))
  (bst-lookup grid (grid-index row col)))

(defun grid-ref (grid index)
  (declare (type fixnum index))
  (bst-lookup grid index))

(defun update-grid (grid row col num)
  (declare (type rc-index row col)
           (type fixnum num))
  (bst-update grid (grid-index row col) num))

(defun bst-range (from to)
  (declare (type fixnum from to))
  (cond
    ((< to from) nil)
    ((= from to) (bst from 0 nil nil))
    (t
     (let ((midpoint (floor (+ from to) 2)))
       (bst midpoint 0
             (bst-range from (- midpoint 1))
             (bst-range (+ midpoint 1) to))))))

(defun puzzle-from-string (string)
  (loop for c across string
       for i from 0
       for bst = (bst-range 0 80) then (bst-update bst i (or (digit-char-p c) 0))
       finally (return bst)))

(defun grid-find (grid value)
  (declare (type fixnum value))
  (and grid
       (or (grid-find (bst-left grid) value)
           (and (= value (bst-value grid)) (bst-key grid))
           (grid-find (bst-right grid) value))))
