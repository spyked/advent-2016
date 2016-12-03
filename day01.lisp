;; Advent of Code 2016, Day 01
(require 'cl-ppcre)
(defvar input)

(defun parse-input (inp)
  ; Break into tokens
  (let ((tokens (cl-ppcre:split "( |,)+" (read-line inp))))
    ; For each token
    (mapcar #'(lambda (token)
                (let ((dirs (subseq token 0 1))
                      (dists (subseq token 1)))
                  (cons (if (string= dirs "R")
                            'right 'left)
                        (parse-integer dists))))
            tokens)))

;; These should be self-explanatory
(defun turn-left (orientation)
  (cdr (assoc orientation
              '((north . west) (west . south)
                (south . east) (east . north)))))

(defun turn-right (orientation)
  (cdr (assoc orientation
              '((north . east) (east . south)
                (south . west) (west . north)))))

(defun move-offset (num-steps orientation)
  (let ((h 0)
        (v 0))
    (case orientation
      (north (incf v num-steps))
      (west (decf h num-steps))
      (south (decf v num-steps))
      (east (incf h num-steps)))
    (values h v)))

;; Go.
(defun go-bibi (input)
  (let ((h 0)
        (v 0)
        (orientation 'north)
        (visited nil)
        (fst-visited-twice nil))
    ; For every step in the instruction manual
    (dolist (instruction input)
      (case (car instruction)
        ; Update orientation
        (left (setq orientation (turn-left orientation)))
        (right (setq orientation (turn-right orientation))))
      ; Visit all positions and keep track of them
      (multiple-value-bind (dh dv)
          (move-offset (cdr instruction) orientation)
        (do ((dh dh)
             (dv dv))
            ((= 0 dh dv))
          (push (cons h v) visited)
          ; Update position
          (when (not (= dh 0))
            (incf h (signum dh)) (decf dh (signum dh)))
          (when (not (= dv 0))
            (incf v (signum dv)) (decf dv (signum dv)))

          ; Check whether we visited
          (if (and (not fst-visited-twice)
                   (member (cons h v) visited :test 'equal))
              (setq fst-visited-twice (cons h v))))))
    ; At the end of the day...
    (values h v orientation fst-visited-twice)))

;; Parse input
(with-open-file (in "day01-input")
  (setq input (parse-input in)))

;(format t "## Debug input: ~S~%" input)
;; Compute distance
(multiple-value-bind (h v _ fst-visited-twice)
    (go-bibi input)
  (declare (ignore _))
  (format t "## Current pos is at (~d,~d), ~d blocks from origin is.~%"
          h v (+ (abs h) (abs v)))
  (format t "## First position visited twice is (~d,~d), ~d blocks from origin.~%"
          (car fst-visited-twice) (cdr fst-visited-twice)
          (+ (abs (car fst-visited-twice)) (abs (cdr fst-visited-twice)))))
