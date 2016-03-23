(asdf:defsystem #:sudoku
    :build-operation asdf:program-op
    :build-pathname "sudoku"
    :entry-point "sudoku:test"
    :serial t
    :components ((:file "package")
                 ;;; XXX (:file "tree-grid")
                 (:file "array-grid")
                 (:file "sudoku")))
