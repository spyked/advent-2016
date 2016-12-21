;; Advent of Code 2016, Day 21
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; The problem's input consists of a series of instructions that operate
; on a given string (a password) to scramble it. The operations are:
;
; - swap position X with position Y: the letter at X is swapped with the
;   one at Y
;
; - swap letter X with letter Y: the letter X is swapped with Y,
;   regardless of their positions
;
; - rotate left/right X steps: rotate the string modulo its length
;
; - rotate based on position of letter X: rotate right based on the
;   position of letter X, i.e. if X has index k, then rotate 1 + k + i,
;   where i = 1 if k >= 4 and i = 0 otherwise
;
; - reverse positions X through Y: reverses the substring from X to Y
;   in-place
;
; - move position X to position Y: remove the letter at X and insert it
;   at Y
;
; All positions start from 0.
;
; For example, given the string abcde, we apply the following operations:
;
; - swap position 4 with position 0 -> ebcda
; - swap letter d with letter b -> edcba
; - reverse positions 0 through 4 -> abcde
; - rotate left 1 step -> bcdea
; - move position 1 to position 4 -> bdeac
; - move position 3 to position 0 -> abdec
; - rotate based on position of letter b -> ecabd
; - rotate based on position of letter d -> decab
;;

;; String operations
;;
;; NOTE: All functions are destructive.
(defun swap-pos (str x y)
  "Swap positions x and y in str."
  (let ((buf (elt str x)))
    (setf (elt str x) (elt str y))
    (setf (elt str y) buf))
  str)

(defun swap-letters (str l1 l2)
  "Swap l1 with l2 in str."
  (let ((pos-l1 (position l1 str))
        (pos-l2 (position l2 str)))
    (swap-pos str pos-l1 pos-l2)))

(defun rotate-general (str offset)
  "General rotation function. Positive offsets rotate right, negative
offsets rotate left."
  (let* ((n (length str))
         (mod-offset (mod offset n))
         (buf (copy-seq (subseq str (- n mod-offset) n))))
    (loop for i from (- n mod-offset 1) downto 0 do
         (setf (elt str (+  mod-offset i)) (elt str i)))
    (setf (subseq str 0 mod-offset) buf))
  str)

(defun rotate-left (str abs-offset)
  "Rotate string left abs-offset positions."
  (rotate-general str (- 0 abs-offset)))

(defun rotate-right (str abs-offset)
  "Rotate string right abs-offset positions."
  (rotate-general str abs-offset))

(defun rotate-pos (str letter)
  "Rotate right based on index of letter in str."
  (let* ((index (position letter str))
         (offset (+ 1 index (if (>= index 4) 1 0))))
    (rotate-right str offset)))

(defun rotate-pos-rev (str letter)
  "Find inverse rotation for rotate-pos"
  (do* ((counter 1 (1+ counter))
        (new-str (rotate-left (copy-seq str) counter)
                 (rotate-left (copy-seq str) counter))
        (tentative (rotate-pos (copy-seq new-str) letter)
                   (rotate-pos (copy-seq new-str) letter))
       (result nil))
      (result result)
    ;(format t "~s ~s~%" new-str tentative)
    (when (string= tentative str)
      (setq result new-str))))

(defun reverse-pos (str x y)
  "Reverse positions x through y in str."
  (setf (subseq str x (1+ y)) (nreverse (subseq str x (1+ y))))
  str)

(defun move-pos (str x y)
  "Move letter at position x to position y."
  (let ((n (length str))
        (victim (elt str x)))
    (setf (subseq str x (- n 1)) (subseq str (1+ x) n))
    (loop for i from (- n 1) downto (1+ y) do
         (setf (elt str i) (elt str (- i 1))))
    (setf (elt str y) victim))
  str)

(defun parse-op (line &optional (in-reverse nil))
  (let* ((numspec "([0-9]+)")
         (letterspec "([a-z])")
         (swap-pos-spec (format nil "swap position ~a with position ~a"
                                numspec numspec))
         (swap-letter-spec (format nil "swap letter ~a with letter ~a"
                                   letterspec letterspec))
         (rotate-left-spec (format nil "rotate left ~a steps?" numspec))
         (rotate-right-spec (format nil "rotate right ~a steps?" numspec))
         (rotate-pos-spec (format nil "rotate based on position of letter ~a"
                                  letterspec))
         (reverse-pos-spec (format nil "reverse positions ~a through ~a"
                                   numspec numspec))
         (move-pos-spec (format nil "move position ~a to position ~a"
                                numspec numspec))
         (spec (format nil "~a|~a|~a|~a|~a|~a|~a"
                       swap-pos-spec swap-letter-spec
                       rotate-left-spec rotate-right-spec
                       rotate-pos-spec reverse-pos-spec move-pos-spec)))
    (multiple-value-bind (_ arr)
        (cl-ppcre:scan-to-strings spec line)
      (declare (ignore _))
      (when arr
        (cond ((and (elt arr 0) (elt arr 1))
               #'(lambda (str)
                   (swap-pos str (parse-integer (elt arr 0))
                                 (parse-integer (elt arr 1)))))
              ((and (elt arr 2) (elt arr 3))
               #'(lambda (str)
                   (swap-letters str (elt (elt arr 2) 0)
                                     (elt (elt arr 3) 0))))
              ((elt arr 4)
               #'(lambda (str)
                   (funcall (if in-reverse
                                #'rotate-right
                                #'rotate-left)
                            str (parse-integer (elt arr 4)))))
              ((elt arr 5)
               #'(lambda (str)
                   (funcall (if in-reverse
                                #'rotate-left
                                #'rotate-right)
                            str (parse-integer (elt arr 5)))))
              ((elt arr 6)
               #'(lambda (str)
                   (funcall
                    (if in-reverse
                        #'rotate-pos-rev
                        #'rotate-pos)
                    str (elt (elt arr 6) 0))))
              ((and (elt arr 7) (elt arr 8))
               #'(lambda (str)
                   (reverse-pos str (parse-integer (elt arr 7))
                                    (parse-integer (elt arr 8)))))
              ((and (elt arr 9) (elt arr 10))
               #'(lambda (str)
                   (if in-reverse
                       (move-pos str
                                 (parse-integer (elt arr 10))
                                 (parse-integer (elt arr 9)))
                       (move-pos str
                                 (parse-integer (elt arr 9))
                                 (parse-integer (elt arr 10)))))))))))

(defun go-bibi (str input-stream &optional (in-reverse nil))
  "Solve part 1."
  (let ((ops (do ((line (read-line input-stream nil)
                        (read-line input-stream nil))
                  (result nil))
                 ((null line) (if in-reverse result (reverse result)))
               (let ((op (parse-op line in-reverse)))
                 (when op
                   (push (cons line op) result))))))
    (dolist (op ops)
      ;(format t "~s: " str)
      (setq str (funcall (cdr op) str))
      ;(format t "~s -> ~s~%" (car op) str)
      ))
  str)
;; Tests
(defvar test-input
  "swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d
")

(let ((test-str "abcde"))
  ; Part 1
  (format t "## Test 0.1: ~a~%" test-str)
  (with-input-from-string (in test-input)
    (setq test-str (go-bibi test-str in)))
  (format t "Scrambles to: ~a~%" test-str)
  ; Part 2
  (format t "## Test 0.2: ~a~%" test-str)
  (with-input-from-string (in test-input)
    (setq test-str (go-bibi test-str in t)))
  (format t "Unscrambles to: ~a~%" test-str))

(let ((my-str "abcdefgh")
      (my-str2 "fbgdceah"))
  ; Part 1
  (format t "## Test 1: ~a~%" my-str)
  (with-open-file (in "day21-input")
    (setq my-str (go-bibi my-str in)))
  (format t "Scrambles to: ~a~%" my-str)
  ; Pre-part 2
  (format t "## Pre-test 2: ~a~%" my-str)
  (with-open-file (in "day21-input")
    (setq my-str (go-bibi my-str in t)))
  (format t "Unscrambles to: ~a~%" my-str)
  ; Part 2
  (format t "## Test 2: ~a~%" my-str2)
  (with-open-file (in "day21-input")
    (setq my-str2 (go-bibi my-str2 in t)))
  (format t "UnScrambles to: ~a~%" my-str2))
