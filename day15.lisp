;; Advent of Code 2016, Day 15
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description
;
; The description on the page is utterly complicated, so I will try to
; simplify it.
;
; We have an object O that at time t=t0 is at place #0, at t=t0 + 1
; is at place #1, etc. until place #n. Each place #i from #1 to
; #n has an integer state S(#i,t) from 0 to k(#i) (k is different for
; each place).
;
; The state S(#i,t) advances modulo k(#i) at each moment in time, and
; the *condition* for O to pass to the next place at time t + 1 is that
; S(#i, t) = 0. The purpose is to find an initial time t0 such that O
; passes through all the places.
;
; Each place is called by the problem a "disc". The initial state of
; each "disc" is given as an input, and it's called by the problem a
; "position", i.e.:
;
; Disc #i has k(#i) positions; at time=t, it is at position j.
;
; For example:
;
; Disc #1 has 5 positions; at time=0, it is at position 4.
; Disc #2 has 2 positions; at time=0, it is at position 1.
;
; The problem asks for the first moment in time when O passes through
; all the places.
;;

; We represent each place at t=0 as a list: (#i k(#i) j); for the
; example above we have the list of states:
;
; ((1 5 4) (2 2 1))

(defun parse-disc (line)
  "Parse a disc according to the problem spec."
  (let ((spec "Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+)."))
    (multiple-value-bind (_ arr)
        (cl-ppcre:scan-to-strings spec line)
      (declare (ignore _))
      (when arr
        (list (parse-integer (elt arr 0))
              (parse-integer (elt arr 1))
              (parse-integer (elt arr 2)))))))

(defun parse-discs (input-stream)
  "Parse all the discs given as input."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (discs nil))
      ((null line) (reverse discs))
    (push (parse-disc line) discs)))

(defun pos (disc time)
  "Find the position of a disk at a given time."
  (mod (+ (caddr disc) time) (cadr disc)))

(defun works (discs time &optional (pos 1))
  "Find whether the problem is satisfied for a given set of discs and a
given initial time."
  (or (> pos (length discs))
      (let ((curr-disc (assoc pos discs)))
        (when (= 0 (pos curr-disc time))
          (works discs (1+ time) (1+ pos))))))

(defun first-time-that-works (discs)
  "Find the first initial time that works.

We return the first moment in time *before* the first position is
reached (i.e. for pos 0)."
  (do ((time 1 (1+ time)))
      ((works discs time) (- time 1))))

;; Tests
(defvar test1.0-input
  "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
")

(let ((discs (with-input-from-string (in test1.0-input)
               (parse-discs in))))
  (format t "## Test 1.0: test1.0-input~%Discs: ~s~%" discs)
  (format t "First time that works is: ~d~%"
          (first-time-that-works discs)))

(let ((discs (with-open-file (in "day15-input")
               (parse-discs in))))
  ; Part 1
  (format t "## Test 1.1: day15-input~%Discs: ~s~%" discs)
  (format t "First time that works is: ~d~%"
          (first-time-that-works discs))
  ; Part 2: the time is reset at t=0 and a new disc with 11 positions
  ; and starting at 0 for time=0 appears exactly after the last disc.
  (let* ((len-discs (length discs))
         (discs2 (append discs (list `(,(+ len-discs 1) 11 0)))))
    (format t "## Test 2.1: day15-input + added cruft~%")
    (format t "Discs: ~s~%" discs2)
    (format t "First time that works is: ~d~%"
            (first-time-that-works discs2))))
