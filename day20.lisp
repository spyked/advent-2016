;; Advent of Code 2016, Day 20
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; We are given a list of numeric ranges, e.g.:
;
; 5-8
; 0-2
; 4-7
;
; And we are asked to find the lowest number that does not fit into any
; range. In this case it's 3.
;;

(defun parse-range (line)
  "Parse a range of numbers."
  (let* ((spec "([0-9]+)-([0-9]+)")
         (arr (multiple-value-bind (_ arr)
                  (cl-ppcre:scan-to-strings spec line)
                (declare (ignore _)) arr)))
    (when arr
      (cons (parse-integer (elt arr 0))
            (parse-integer (elt arr 1))))))

(defun parse-input (input-stream)
  "Read a list of ranges from input-stream.

The order of the ranges is not guaranteed."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (result nil))
      ((null line) result)
      (push (parse-range line) result)))

; The problem is that there is a high number of ranges of a lot higher
; values (i.e. 2^32), so how to solve this efficiently? Let's try
; sorting.
(defun sort-ranges (ranges)
  (sort ranges #'(lambda (range1 range2)
                   (< (car range1) (car range2)))))

; Now let's try and check them two by two. For example, we take 0-2 and
; 4-7. Given that 2+1 is strictly smaller than 4, then there is a gap
; between the two ranges, so 2+1 = 3 is the first number that's not in
; any range.
(defun disjoint-ranges? (range1 range2)
  "Are the two ranges strictly disjoint?"
  (< (1+ (cdr range1)) (car range2)))

(defun check-ranges (ranges)
  "Check a sorted set of ranges for strict disjointdness (as we've
defined it) and return the first number that does not fall into any
range."
  (do ((r1 ranges (cdr r1))
       (r2 (cdr ranges) (cdr r2))
       (result nil))
      ((or result (null r2)) result)
    (when (disjoint-ranges? (car r1) (car r2))
      (setq result (1+ (cdar r1))))))

;; Part2: Wants *all* the numbers that are not in any ranges.
(defun check-all-ranges (ranges)
  "Check a sorted set of ranges for strict disjointdness (as we've
defined it) and return all the numbers that don't fall in any range."
  (do* ((r2 (cdr ranges) (cdr r2))
        (last-blocked (car ranges))
        (result 0))
       ((null r2) result)
    (when (disjoint-ranges? last-blocked (car r2))
      ;(format t "Found disjoint: ~d ~s" last-blocked (car r2))
      (incf result (- (caar r2) (cdr last-blocked) 1))
      ;(format t " ~d~%" (- (caar r2) (cdr last-blocked) 1))
      )
    (setf (cdr last-blocked) (max (cdr last-blocked) (cdar r2)))))

(defun make-sorted-ranges (ranges max)
  (nconc (list (cons -1 -1))
         (sort-ranges ranges)
         (list (cons (1+ max) (1+ max)))))

;; Tests
(defvar test-input
  "5-8
0-2
4-7")

(let ((ranges (with-input-from-string (in test-input)
                (parse-input in))))
  (setq ranges (make-sorted-ranges ranges 9))
  (format t "## Test 0.1: ranges from test-input~%")
  (format t "~s~%" (check-ranges ranges))

  ; Part 2
  (let ((nums (check-all-ranges ranges)))
   (format t "## Test 0.2: ranges from test-input~%")
   (format t "total ~d~%" nums)))

(let ((ranges (with-open-file (in "day20-input")
                (parse-input in))))
  (setq ranges (make-sorted-ranges ranges 4294967295))
  (format t "## Test 1: ranges from day20-input~%")
  (format t "~s~%" (check-ranges ranges))

  ; Part 2
  (let ((nums (check-all-ranges ranges)))
    (format t "## Test 2: ranges from day20-input~%")
    (format t "total ~d~%" nums)))
