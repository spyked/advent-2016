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

(defun circlen (circ-list)
  "Returns the length of a circular list.

Assumes that the list is non-null and that the elements in the list are
unique."
  (do ((head circ-list)
       (rest (cdr circ-list) (cdr rest))
       (counter 1 (1+ counter)))
      ((equal (car head) (car rest)) counter)))

(defun make-elves (n)
  "Make a list of elves."
  (let ((elves (loop for i from 1 to n
                  collect (cons i 1))))
    (setf (cdr (last elves)) elves)
    elves))

(defun iterate (elves)
  "Solve part 1."
  (do ((e elves (cdr e))
       (fin nil))
      ((or fin (circlen-is-1? e)) e)
    (let ((victim (cdr e)))
      ;(format t "Elf ~s takes from ~s.~%" (car e) (car victim))
      (incf (cdar e) (cdar victim))      
      (setf (cdr e) (cddr e))
      ;(setq fin t)
      )))

;; Part 2
;
; We "steal from elves" the same way, only the removal strategy is
; different: we remove elves from *across* the circle, i.e. diametrally
; opposite. So assuming that the length of the circle is given by the
; number of elves, then we choose the elf that is n / 2 places away from
; us.
;
; For example:
;
;   1
; 5   2
;  4 3
;
; - notice that in this case there are two elves in front of 1, but 5 /
;   2 = 2, and the elf 2 places away from us is 3, so the new circle
;   becomes:
;
;   1
; 5   2
;   4
;
; - now 2 gets to steal (4/2 = 2 => from 5), so the circle becomes:
;
;   1
;  4 2
;
; - 4 gets to steal from 3/2 = 1 position away, that being 1, so we
;   remain with:
;
;  2
;  4
;
; - 2 gets to steal next from 2/2 = 1 position away, so 2 is the winner.
;;

(defun find-elf-across (elves)
  "Find elf across the current elf.

The first elf returned is the elf *before* it, so we can remove the
actual elf from the list."
  (let ((num-pos (floor (/ (circlen elves) 2))))
    (do ((k 1 (1+ k))
         (e elves (cdr e)))
        ((= k num-pos) e))))

(defun iterate2 (elves n)
  "Solve part 2."
  (let ((e elves)
        (bef-victim (find-elf-across elves)))
    (do ((fin nil)
         (counter n (- counter 1)))
        ((or fin (circlen-is-1? e)) e)
      (let* ((victim (cdr bef-victim)))
        ;; (when (= 0 (mod counter 1000))
        ;;  (format t "Elf ~s takes from ~s.~%" (car e) (car victim)))

        ; Update presents, remove victim
        (incf (cdar e) (cdar victim))
        (setf (cdr bef-victim) (cdr victim))

        ; Update elves: we only update the victim once two iterations
        ; (empirically that seems to be the case, it can probably be
        ; demonstrated inductively).
        (setq e (cdr e))
        (when (= 1 (mod counter 2))
         (setq bef-victim (cdr bef-victim)))
        ;(setq fin t)
        ))))

;; Tests
(let* ((test-input 5)
       (elves (make-elves test-input)))
  (format t "## Test 0.1: ~d elves.~%" test-input)
  (format t "Winning elf is ~s.~%" (iterate elves)))

(let* ((test-input 5)
       (elves (make-elves test-input)))
  (format t "## Test 0.2: ~d elves.~%" test-input)
  (format t "Winning elf is ~s.~%" (iterate2 elves test-input)))

(let* ((my-input 3018458)
       (elves (make-elves my-input)))
  (format t "## Test 1: ~d elves.~%" my-input)
  (format t "Winning elf is ~s.~%" (iterate elves)))

(let* ((my-input 3018458)
       (elves (make-elves my-input)))
  (format t "## Test 2: ~d elves.~%" my-input)
  (format t "Winning elf is ~s.~%" (iterate2 elves my-input)))
