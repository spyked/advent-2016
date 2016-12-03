;; Advent of Code 2016, Day 02

;; Problem description:
;
; A numeric keypad is given:
;
; 1 * 2 * 3
; * * * * *
; 4 * 5 * 6
; * * * * *
; 7 * 8 * 9
;
; and the unlock code is given as a set of instructions how to move the
; finger on the keypad: (U)p, (D)own, (L)eft or (R)ight, starting from
; position "5". The input is given as a sequence of lines, each end of
; line denoting that the current number must be pressed.
;
; Let's take the example given by the creators:
;
; ULL
; RRDDD
; LURDL
; UUUUD
;
; gets translated into:
;
; - start from 5, go U (2), L (1), L (still 1, no effect), press 1
; - start from 1, go R (2), R (3), D (6), D (9), D (9), press 9
; - start from 9, go L (8), U (5), R (6), D (9), L (8), press 8
; - start from 8, go U (5), U (2), U (2), U (2), D (5), press 5
;
; so the final code is 1985.
;;

;; Part 1: State machine
(deftype rowcol () '(integer 0 2))
(defstruct state-machine
  (col 1 :type rowcol)
  (row 1 :type rowcol))

; Some helpful cast macros
(defmacro ->integer (n) `(coerce ,n 'integer))
(defmacro string->list (S) `(coerce ,S 'list))
(defmacro list->string (L) `(coerce ,L 'string))

(defun machine->digit (machine)
  (+ (* 3 (->integer (state-machine-row machine)))
     (->integer (state-machine-col machine))
     1))

;; Primitive machine functions
(defun go-direction (direction machine)
  (case direction
    (#\U (setf (state-machine-row machine)
               (max 0 (- (state-machine-row machine) 1))))
    (#\D (setf (state-machine-row machine)
               (min 2 (+ (state-machine-row machine) 1))))
    (#\L (setf (state-machine-col machine)
               (max 0 (- (state-machine-col machine) 1))))
    (#\R (setf (state-machine-col machine)
               (min 2 (+ (state-machine-col machine) 1)))))
  machine)

;; Determine code from instructions
(defun go-bibi (input-stream)
  (do ((code 0)
       (machine (make-state-machine))
       (line (read-line input-stream nil) (read-line input-stream nil)))
      ((null line) code)
    (dolist (direction (string->list line))
      (go-direction direction machine))
    (setq code (+ (* 10 code) (machine->digit machine)))))

;; Part 2
; 
; The instructions are the same, but the keypad has a different shape:
;
;         1
;       * * *
;     2 * 3 * 4
;   * * * * * * *
; 5 * 6 * 7 * 8 * 9
;   * * * * * * *
;     A * B * C
;       * * *
;         D
;
; We still start at "5", but going U, D, L or R has different meanings:
;
; - ULL leaves us at 5
; - RRDDD: 5 -> 6 -> 7 -> B -> D -> D
; - LURDL: D -> D -> B -> C -> C -> B
; - UUUUD: B -> 7 -> 3 -> 1 -> 1 -> 3
;
; So the final code is 5DB3.
;;

; Some discussion: how do we represent the coordinates? How do we
; compute the digit based on that?
;
; We can still use cols and rows, but it's convenient to set "7" as the
; origin and, on moves, ensure that the following invariant holds:
;
; (< (+ (abs col) (abs row)) 3)
;
; Digits are base-13, so we compute them using the following formula:
;
; (+ 7 (* (sum k (1 (abs row)) (* 2 (- 3 (abs k)))) (signum row)) col)
;
; this is a very weird formula, where 7 is the origin number
; (adjustment), 2 comes from the rhombus shape (* 2 number of elements
; on each side of the rhombus) and 3 comes from the rhombus' size. Sum
; is actually a loop statement.
 
;; Part 2: State machine
(defstruct state-machine2
  (col -2 :type integer)
  (row 0 :type integer))

;; Get digit
(defun machine2->digit (machine)
  (let* ((col (state-machine2-col machine))
         (row (state-machine2-row machine))
         (digit-decimal
          (+ 7 (* (loop for k from 1 to (abs row)
                    sum (* 2 (- 3 (abs k)))) (signum row))
             col)))
    (elt "0123456789ABCD" digit-decimal)))

;; Go in a given direction
(defun go-direction2 (direction machine)
  (let ((try-machine (make-state-machine2 :col (state-machine2-col machine)
                                          :row (state-machine2-row machine))))
    (case direction
      (#\U (decf (state-machine2-row try-machine)))
      (#\D (incf (state-machine2-row try-machine)))
      (#\L (decf (state-machine2-col try-machine)))
      (#\R (incf (state-machine2-col try-machine))))
    (when (< (+ (abs (state-machine2-row try-machine))
                (abs (state-machine2-col try-machine)))
             3)
        (setf (state-machine2-row machine) (state-machine2-row try-machine)
              (state-machine2-col machine) (state-machine2-col try-machine))))
  machine)

;; Determine code from instructions
(defun go-bibi2 (input-stream)
  (do ((code nil)
       (machine (make-state-machine2))
       (line (read-line input-stream nil) (read-line input-stream nil)))
      ((null line) (list->string (reverse code)))
    (dolist (direction (string->list line))
      (go-direction2 direction machine))
    (print machine)
    (push (machine2->digit machine) code)))
      
;; Tests
(let ((str (format nil "ULL~%RRDDD~%LURDL~%UUUUD")))
  (format t "## Test 0:~%~S~%" str)
  (with-input-from-string (in str)
    (format t "## Code for 0.1: ~S~%" (go-bibi in)))
  (with-input-from-string (in str)
    (format t "## Code for 0.2: ~S~%" (go-bibi2 in))))

(with-open-file (in "day02-input")
  (format t "## Test 1: input from day02-input~%Code: ~S~%"
          (go-bibi in)))
(with-open-file (in "day02-input")
  (format t "## Test 2: input from day02-input~%Code: ~S~%"
          (go-bibi2 in)))
