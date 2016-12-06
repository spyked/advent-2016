;; Advent of Code 2016, Day 05
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :sb-md5)) ; my choice of md5 digest implementation

;; Problem description:
;
; The problem asks to find a so-called eight-character "password" from a
; given input.
;
; The password is found by md5-summing the input concatenated with a
; number, e.g. if the input is "abc", then we need to md5-sum "abc0",
; "abc1", "abc2" and so on. A character is found when we obtain a
; md5-sum that contains five leading zeroes, the character we seek being
; the sixth character of the md5-sum. We iterate the number until we
; find eight such characters.
;
; For example, given the input "abc":
;
; - the first character is obtained for "abc3231929", the sixth
;   character of the hash being "1".
;
; - the second character is obtained for "abc5017308", the sixth
;   character of the hash being "8".
;
; - the third character is obtained for "abc5278568", the sixth
;   character of the hash being "f".
;
; and so on until we find the final password, 18f47a30.
;;

; The puzzle input is short, so we don't place it into a file.
(defvar day05-input "uqwqemis")

(defun get-md5-hash (str)
  "Obtain the MD5 hash of str as a string."
  ; md5sum-string returns an array of bytes, which we turn into a list
  ; and display using hex; then we concatenate the result.
  (sb-md5:md5sum-string str))

(defun make-input (id iterator)
  "Craft an input to find password."
  (concatenate 'string id (format nil "~d" iterator)))

(defmacro debug-tentative (tentative)
  `(print (mapcar #'(lambda (b) (format nil "~(~x~)" b))
                  (coerce ,tentative 'list))))

(defun get-character (tentative)
  "Get the character from a tentative md5-sum string."
  (let ((has-leading-zeroes
         (and (= 0 (elt tentative 0))
              (= 0 (elt tentative 1))
              (> 16 (elt tentative 2)))))
    (when has-leading-zeroes
      (elt (format nil "~(~2,'0x~)" (elt tentative 2)) 1))))

(defun go-bibi (input)
  "Solve the problem (Part 1)."
  (do* ((counter 0 (1+ counter))
        (pass-list nil)
        (hash (get-md5-hash (make-input input counter))
              (get-md5-hash (make-input input counter))))
       ((= 8 (length pass-list))
        (coerce (reverse pass-list) 'string))
    (let ((chr (get-character hash)))
      (when chr
        (format t "Found character: ~s~%" chr)
        (push chr pass-list)))))

;; Part 2:
;
; This is the same as part 1, only the hash interpretation is
; different. The five first characters must be 0, the sixth must
; represent a position between 0 and 7, and in this case the seventh
; character is the character on the given position. We use only the
; first result found.
;
; For our previous example, "abc":
;
; - for "abc3231929", the hash is 0000015..., so the position is 1 and
;   the character is 5
;
; - for "abc5017308", the position is invalid, 8, so we ignore it
;
; - for "abc5357525', the result is 000004e..., so the position is 4 and
;   the character is e
;;

(defun array->hexstring (arr)
  "Convert an array to a hexstring."
  (apply #'concatenate 'string
         (mapcar #'(lambda (b) (format nil "~(~x~)" b))
                 (coerce arr 'list))))

(defun get-character2 (tentative)
  "Get character (actually, a char-pos pair) according to Part 2 spec."
  (let ((is-valid-spec
         (and (= 0 (elt tentative 0))
              (= 0 (elt tentative 1))
              (> 8 (elt tentative 2)))))
    (when is-valid-spec
      (cons (elt (format nil "~(~2,'0x~)" (elt tentative 3)) 0)
            (elt tentative 2)))))

(defmacro done2 (pass-arr)
  "Check if we're done, i.e. all the elements in pass-arr are non-nil."
  (let ((counter (gensym))
        (stop (gensym)))
    `(do ((,counter 0 (1+ ,counter))
          (,stop nil))
         ((or ,stop (= ,counter 8)) (not ,stop))
       (when (not (elt ,pass-arr ,counter)) (setq ,stop t)))))

(defun go-bibi2 (input)
  "Solve Part 2 of the problem."
  (do* ((counter 0 (1+ counter))
        (pass-arr (make-array 8 :initial-element nil))
        (hash (get-md5-hash (make-input input counter))
              (get-md5-hash (make-input input counter))))
       ((done2 pass-arr) (array->hexstring pass-arr))
    (let ((p (get-character2 hash)))
      ; p is a (char . pos) pair or nil
      (when (and p (not (elt pass-arr (cdr p))))
        (format t "Found char: ~s~%" p)
        (setf (elt pass-arr (cdr p)) (car p))))))

;; Tests
;; (format t "## Test 0: input = \"abc\", password = ~a~%"
;;         (go-bibi "abc"))
;
; To run part1: (go-bibi day05-input)
