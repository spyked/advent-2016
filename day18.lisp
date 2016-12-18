;; Advent of Code 2016, Day 18

;; Problem description
;
; Cellular automata ftw! We have to iterate through a simple 1D cellular
; automaton: a floor of tiles that can be either "safe" or "traps" and
; whose *rows* depend on values of *previous rows*.
;
; The puzzle input gives a row for which #\. denotes a safe tile and #\^
; denotes an unsafe tile (i.e. a trap). We will translate these to
; zeroes and ones respectively, i.e. ..^^. is equivalent to 00110. So
; basically we have the truth value of whether a tile is a trap, denoted
; as a number.
;
; Then our cellular automaton rule (described on the site) becomes:
;
; - 000 -> 0
; - 001 -> 1
; - 010 -> 0
; - 011 -> 1
; - 100 -> 1
; - 101 -> 0
; - 110 -> 1
; - 111 -> 0
;
; So basically rule 90, which is a xor between the left tile and the
; right tile (the center tile really doesn't matter).
;
; Note that edge tiles are considered implicitly safe (i.e. 0).
;
; So for example ..^^. is 00110, which "evolves" like this across
; iterations:
;
; - 0: 00110
; - 1: 01111
; - 2: 11001
; - 3: 11110
; and so on.
;
; Further examples are given in the tests.
;
; The problem asks us how many *safe* (i.e. 0) tiles there are starting
; from the input after 40 iterations.
;;

;; General utility functions
(defmacro second-value (expr)
  "Get the second value in an expr that returns multiple values."
  (let ((fst (gensym))
        (snd (gensym)))
   `(multiple-value-bind (,fst ,snd)
        ,expr
      (declare (ignore ,fst))
      ,snd)))

(defun xor (x y)
  "XOR two numbers, assuming modulo-2 integer arithmetic."
  (let* ((x-bit0 (second-value (truncate x 2)))
         (y-bit0 (second-value (truncate y 2))))
    (second-value (truncate (+ (* x-bit0 (- 1 y-bit0))
                               (* (- 1 x-bit0) y-bit0))
                            2))))

(defun count-zeroes (sequence)
  "Count the zeroes in a sequence."
  (reduce #'(lambda (acc el)
              (if (= el 0) (1+ acc) acc))
          sequence :initial-value 0 ))

;; Functions on CAs
(defun string->ca (line)
  "Make a cellular automaton from an line given as a string."
  (map 'vector #'(lambda (c)
                   (case c
                     (#\. 0)
                     (#\^ 1)))
       line))

(defun ca->string (row)
  "Make an input string from a cellular automaton."
  (map 'string #'(lambda (n)
                   (case n
                     (0 #\.)
                     (1 #\^)))
       row))

(defun ca-boundary (row)
  "Returns the rightmost boundary of a CA."
  (- (array-dimension row 0) 1))

(defun get-tile (row i)
  "Get tile i from row. Non-existing tiles are assumed to be 0."
  (if (or (< i 0) (> i (ca-boundary row)))
      0
      (elt row i)))

(defun rule-90 (row)
  "Apply rule 90 to a CA row."
  (coerce (loop for i from 0 to (ca-boundary row) collect
               (xor (get-tile row (- i 1))
                    (get-tile row (+ i 1))))
          'vector))

;; Let's be really nice and make a general mechanism to apply a rule
;; recursively, yielding a stream.
(defun ca-stream (initial rule)
  "Build a stream out of an initial condition and a CA rule."
  (cons initial #'(lambda ()
                    (ca-stream (funcall rule initial) rule))))

(defun eval-stream (n stream)
  "Eval the first n elements of a given stream."
  (if (= n 0)
      stream
      (cons (car stream) (eval-stream (- n 1) (funcall (cdr stream))))))

(defun nth-stream (n stream)
  "Return the nth element in a stream."
  (do ((s (eval-stream n stream) (cdr s))
       (k n (- k 1)))
      ((= k 0) (car s))))

(defun take-stream (n stream)
  "Return the nth element in a stream."
  (do ((s (eval-stream n stream) (cdr s))
       (k n (- k 1))
       (l nil))
      ((= k 0) (reverse l))
    (push (car s) l)))

(defun reduce-stream (n stream func &key initial-value)
  "Reduce a stream according to a given function, iterating n times."
  (do ((s stream (cdr (eval-stream 1 s)))
       (k n (- k 1))
       (acc initial-value))
      ((= k 0) acc)
    (setq acc (funcall func acc (car s)))))

;; Tests
(let* ((initial ".^^.^.^^^^")
       (example-ca (ca-stream (string->ca initial) #'rule-90)))
  (format t "## Example: Rule 90 CA with 10 iterations.~%")
  (loop for row in (take-stream 10 example-ca) do
       (format t "~a~%" (ca->string row)))
  (format t "##~%"))

(let* ((initial (with-open-file (in "day18-input") (read-line in)))
       (my-ca (ca-stream (string->ca initial) #'rule-90))
       (sum 0))
  ;; Part 1
  (format t "## My input: from day18-input, 40 iterations.~%")
  (loop for row in (take-stream 40 my-ca) do
       (format t "~a~%" (ca->string row))
       (incf sum (count-zeroes row)))
  (format t "~%After 40 iterations, there are a total of ~d safe tiles.~%"
          sum)

  ;; Part 2
  (setq sum (reduce-stream 400000 my-ca
                           #'(lambda (acc el)
                               (+ acc (count-zeroes el)))
                           :initial-value 0))
  (format t "~%After 400000 iterations, there are a total of ~d safe tiles.~%"
          sum))
