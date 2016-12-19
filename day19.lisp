;; Advent of Code 2016, Day 19

;; Problem description:
;
; We have a circular array of "elves", each with one "present", i.e. #i
; for i = 0 to n - 1, present(#i) = 1. n is the puzzle input.
;
; There is an iterative algorithm going on:
;
; for each elf #i for which present(#i) > 0
;   - find next elf j (> i, modulo n) so that present(#j) > 0
;   - if j = i then stop, else:
;   - present(#i) += present(#j)
;   - present(#j) = 0
;
; At the end we need to find the only i for which present(#i) > 0.
;;
; Detect cycles when printing.
(setf *print-circle* t)

(defun circlen-is-1? (circ-list)
  "Returns whether a circular list has exactly one element."
  (equal (car circ-list) (cadr circ-list)))

(defun make-elves (n)
  "Make a list of elves."
  (let ((elves (loop for i from 1 to n
                  collect (cons i 1))))
    (setf (cdr (last elves)) elves)
    elves))

(defun find-next-elf-left (elves)
  "Find the next elf to the left, starting from the current elf."
  (do ((e (cdr elves) (cdr elves)))
      ((> (cdar elves) 0) e)))

(defun iterate (elves)
  (do ((e elves (find-next-elf-left e))
       (fin nil))
      ((or fin (circlen-is-1? e)) e)
    (let ((victim (cdr e)))
      ;(format t "Elf ~s takes from ~s.~%" (car e) (car victim))
      (incf (cdar e) (cdar victim))      
      (setf (cdr e) (cddr e))
      ;(setq fin t)
      )))

;; Part 2: TODO

;; Tests
(let* ((test-input 5)
       (elves (make-elves test-input)))
  (format t "## Test 0.1: ~d elves.~%" test-input)
  (format t "Winning elf is ~s.~%" (iterate elves)))

(let* ((my-input 3018458)
       (elves (make-elves my-input)))
  (format t "## Test 1: ~d elves.~%" my-input)
  (format t "Winning elf is ~s.~%" (iterate elves)))
