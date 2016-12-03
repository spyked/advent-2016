;; Advent of Code 2016, Day 03

;; Problem description:
;
; Simplest problem yet. Given a sequence of triples of numbers,
; determine if each triple forms a triangle, i.e. considering each
; number represents the length of a triangle's side. Count the number of
; triples that form a triangle.
;
; The input is organised as follows. Each line contains a triple. The
; numbers in a triple are separated by spaces.
;
; For a triple to form a triangle, the sum of any of the two sides must
; be strictly larger than the remaining side.
;
; Example: given, 5 10 25, we check for each combination that (> (+ n1
; n2) n3). This is not true for 5, 10 and 25, so the triple is not a
; triangle.
;;

;; Functions
(defun parse-line (line)
  "Parses a string of the form \"n1 n2 n3\", where n1, n2 and n3 are
numbers separated by spaces."
  (with-input-from-string (in line)
    (let ((first (read in))
          (second (read in))
          (third (read in)))
      (list first second third))))

(defun is-triangle (L)
  "Checks whether a given triple forms a triangle."
  (let ((s1 (car L))
        (s2 (cadr L))
        (s3 (caddr L))
        (cmp #'(lambda (s1 s2 s3)
                 (> (+ s1 s2) s3))))
    (and (funcall cmp s1 s2 s3)
         (funcall cmp s2 s3 s1)
         (funcall cmp s3 s1 s2))))

(defun go-bibi (input-stream)
  "Counts the number of triples (as given according to the input
description) for which the triangle relation holds."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (count 0)
       (total 0))
      ((null line) (values count total))
    (let ((sides (parse-line line)))
      (when (is-triangle sides)
        (format t "~s~%" sides)
        (incf count))
      (incf total))))

;; Part 2
;
; Someone changed their mind. It seems that rows are unrelated, and each
; groups of three on the same column must be analyzed using the same
; algorithm as above.

(defun group-sides (L1 L2 L3)
  "Transposes a 3x3 matrix of sides."
  (values (list (car L1) (car L2) (car L3))
          (list (cadr L1) (cadr L2) (cadr L3))
          (list (caddr L1) (caddr L2) (caddr L3))))

(defun go-bibi2 (input-stream)
  "Same as go-bibi, but read sides as groups of three numbers on the
same column."
  ; Read lines -- assume that the number of lines in input-stream is
  ; divisible by 3!
  (do ((line1 (read-line input-stream nil) (read-line input-stream nil))
       (line2 (read-line input-stream nil) (read-line input-stream nil))
       (line3 (read-line input-stream nil) (read-line input-stream nil))
       (count 0)
       (total 0))
      ((or (null line1) (null line2) (null line3)) (values count total))
    (multiple-value-bind (sides1 sides2 sides3)
        (group-sides (parse-line line1) (parse-line line2) (parse-line line3))
      (dolist (sides (list sides1 sides2 sides3))
        (when (is-triangle sides)
          (incf count))
        (incf total)))))

;; Tests
(with-input-from-string (in "10 5 25")
  (format t "## Test 0:~%Input:~%10 5 25~%Num triangles: ~d~%"
          (go-bibi in)))

(with-open-file (in "day03-input")
  (format t "## Test 1:~%Input from day03-input~%Num triangles: ~d~%"
          (go-bibi in)))

(with-open-file (in "day03-input")
  (format t "## Test2:~%Input from day03-input~%Num triangles: ~d~%"
          (go-bibi2 in)))
