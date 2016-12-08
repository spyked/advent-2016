;; Advent of Code 2016, Day 08
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; The problem state is represented by a 50x6 (width x height) grid of
; pixels that are initially *turned off*.
;
; We can alter the state of the grid using the following commands:
;
; - rect AxB: turns on the first AxB pixels (in a rectangle) starting
;   from the upper-left corner of the grid.
;
; - rotate row y=A by B: selects row A and rotates its pixels right by B
;   (wrapping around the edges of the grid)
;
; - rotate column x=A by B: selects column A and rotates its pixels down
;   by B (wrapping around the edges of the grid)
;
; Let's take the following example:
;
; - rect 3x2
;
;   ###....
;   ###....
;   .......
;
; - rotate column x=1 by 1
;
;   #.#....
;   ###....
;   .#.....
;
; - rotate row y=0 by 4
;
;   ....#.#
;   ###....
;   .#.....
;
; - rotate column x=1 by 1 (notice how the pixel wraps around)
;
;   .#..#.#
;   #.#....
;   .#.....
;
; The commands are given as input, one command on each line.
;
; At the end of the string of commands, we need to find out how many
; pixels are turned on.
;;

; We have two problems here: parsing the command input; and representing
; the grid and manipulating it. We can represent the grid as an array.
;
; A command has the form: (#'func-name params*), where func-name is a
; function whose first parameter is the grid and params* are the other
; parameter commands. For example the command (#'make-rect 3 2) will
; actually be issued as (funcall #'make-rect grid 3 2).
;
; A grid is a bidimensional array of given dimensions (the test examples
; have a different size spec than the one given in the problem
; statement). The grid's element types are nil (for a pixel that's
; turned off) or t (for a pixel that's turned on).

(declaim (ftype function rect rotate-row rotate-col))

(defun parse-command (line)
  "Parse commands according to the problem description."
  (let* ((rect-regexp "rect +([0-9]+)x([0-9]+)")
         (rrow-regexp "rotate row +y=([0-9]+) +by +([0-9]+)")
         (rcol-regexp "rotate column +x=([0-9]+) +by +([0-9]+)")
         (regexp (format nil "~a|~a|~a"
                         rect-regexp rrow-regexp rcol-regexp))
         (scan (cadr (multiple-value-list
                      (cl-ppcre:scan-to-strings regexp line)))))
    (when scan
      (cond
        ((and (elt scan 0) (elt scan 1)) ; rect
         `(,#'rect ,(parse-integer (elt scan 0))
                  ,(parse-integer (elt scan 1))))
        ((and (elt scan 2) (elt scan 3)) ; rotate row
         `(,#'rotate-row ,(parse-integer (elt scan 2))
                        ,(parse-integer (elt scan 3))))
        ((and (elt scan 4) (elt scan 5)) ; rotate col
         `(,#'rotate-col ,(parse-integer (elt scan 4))
                        ,(parse-integer (elt scan 5))))))))

(defun make-grid (x y)
  "Make a grid of x columns and y rows, with all pixels turned off."
  (make-array `(,y ,x) :initial-element nil))

(defun light-up (grid x y)
  "Light up pixels on column x, row y of grid."
  (setf (aref grid y x) t)
  grid)

(defun rect (grid x y)
  "Light up pixels form 0,0 to x,y on grid, in a rectangular fashion."
  (loop for i from 0 to (- x 1) do
       (loop for j from 0 to (- y 1) do
            (light-up grid i j)))
  grid)

(defun rotate-row (grid x y)
  "Rotate row x on grid by y positions."
  (let* ((ay (mod y (array-dimension grid 1)))
         (last-col-index (- (array-dimension grid 1) 1))
         (buffer (make-array ay)))
    ; Buffer elements from last-col-index - y + 1 to last-col-index
    (loop for i from (- last-col-index ay -1) to last-col-index do
         (setf (aref buffer (- last-col-index i)) (aref grid x i)))
    ; Shift elements from last-col-index - y to 0 right by y
    (loop for i from (- last-col-index ay) downto 0 do
         (setf (aref grid x (+ ay i)) (aref grid x i)))
    ; Populate back elements from buffer, in reverse
    (loop for i from 0 to (- ay 1) do
         (setf (aref grid x (- ay i 1)) (aref buffer i)))
    )
  grid)

(defun rotate-col (grid y x)
  "Rotate column y on grid by x positions."
  (let* ((ax (mod x (array-dimension grid 0)))
         (last-row-index (- (array-dimension grid 0) 1))
         (buffer (make-array ax)))
    ; Buffer elements from last-row-index - x + 1 to last-row-index
    (loop for i from (- last-row-index ax -1) to last-row-index do
         (setf (aref buffer (- last-row-index i)) (aref grid i y)))
    ; Shift elements from last-row-index - x to 0 down by x
    (loop for i from (- last-row-index ax) downto 0 do
         (setf (aref grid (+ ax i) y) (aref grid i y)))
    ; Populate back elements from buffer, in reverse
    (loop for i from 0 to (- ax 1) do
         (setf (aref grid (- ax i 1) y) (aref buffer i)))
    )
  grid)

(defun count-on (grid)
  (let ((counter 0))
   (loop for x from 0 to (- (array-dimension grid 0) 1) do
        (loop for y from 0 to (- (array-dimension grid 1) 1) do
             (when (aref grid x y) (incf counter))))
   counter))

(defun print-grid (grid)
  "Print grid to standard output."
  (loop for x from 0 to (- (array-dimension grid 0) 1) do
       (loop for y from 0 to (- (array-dimension grid 1) 1) do
            (format t "~a" (if (aref grid x y) "#" "."))
            (when (= (1+ y) (array-dimension grid 1))
              (format t "~%"))))
  (format t "~%"))

(defun issue-command (grid command)
  "Issue command on grid, as parsed by parse-command."
  (apply (car command) grid (cdr command))
  grid)

(defun go-bibi (input-stream x y)
  "Solve problem. x and y are parameters to make-grid."
  (let ((grid (make-grid x y)))
    ;(print-grid grid)
    (do ((line (read-line input-stream nil) (read-line input-stream nil)))
        ((null line))      
      (issue-command grid (parse-command line))
      ;(format t "Command: ~a~%" line)
      ;(print-grid grid)
      )
    (values grid (count-on grid))))

;; Tests
(defvar test0-input
  (format nil "~a~%~a~%~a~%~a~%"
          "rect 3x2"
          "rotate column x=1 by 1"
          "rotate row y=0 by 4"
          "rotate column x=1 by 1"))
(with-input-from-string (in test0-input)
  (multiple-value-bind (grid num)
      (go-bibi in 7 3)
    (format t "## Test 0: from test0-input, ~d pixels.~%" num)
    (format t "Grid:~%")
    (print-grid grid)))

(with-open-file (in "day08-input")
  (multiple-value-bind (grid num)
      (go-bibi in 50 6)
    (format t "## Test 1: from day08-input, ~d pixels.~%" num)
    (format t "Grid:~%")
    (print-grid grid)))
