;; Advent of Code 2016, Day 09

;; Problem description:
;
; The input is a compressed file, which we must decompress and count the
; number of characters, ignoring whitespaces.
;
; The compression scheme works as follows: read characters, ignoring
; whitespace; when encountering a sequence of the form (AxB), repeat the
; number A of characters after this sequence (a so-called "data frame")
; B times. The data frame may contain parentheses, but in this case we
; treat them as regular characters.
;
; Examples:
;
; - ADVENT "decompresses" to ADVENT.
;
; - A(1x5)BC decompresses to ABBBBBC.
;
; - (3x3)XYZ decompresses to XYZXYZXYZ.
;
; - A(2x2)BCD(2x2)EFG decompresses to ABCBCDEFEFG.
;
; - (6x1)(1x3)A decompresses to (1x3)A.
;
; - X(8x2)(3x3)ABCY decompresses to X(3x3)ABC(3x3)ABCY.
;;

(defmacro new-string ()
  "Make a new empty string to be used as an output stream."
  `(make-array '(0)
               :element-type 'base-char
               :fill-pointer 0
               :adjustable t))

(defun parse-decoder (input-stream)
  "Parse a decompressor decoder of the form AxB)."
  (let ((a-str (new-string))
        (b-str (new-string)))
    (with-output-to-string (out a-str)
      (do ((c (read-char input-stream nil) (read-char input-stream nil)))
          ((eq #\x c))
        (write-char c out)))
    (with-output-to-string (out b-str)
      (do ((c (read-char input-stream nil) (read-char input-stream nil)))
          ((eq #\) c))
        (write-char c out)))
    (cons (parse-integer a-str) (parse-integer b-str))))

(defun decompress (input-stream)
  "Decompress input-stream."
  (do ((ret (new-string))
       (c (read-char input-stream nil) (read-char input-stream nil)))
      ((null c) ret)
    ; Clunky state machine stepper, but it works.
    (with-output-to-string (out ret)
     (case c
       ((#\Space #\Newline) #| Ignore this |#)
       (#\( (let ((decoder (parse-decoder input-stream))
                  (data-frame (new-string)))
              ; Parse decoder
              (with-output-to-string (df-out data-frame)
                ; Parse data frame
                (loop for i from 1 to (car decoder) do
                     (write-char (read-char input-stream) df-out)))
              ; Write data frame B times
              (loop for i from 1 to (cdr decoder) do
                   (write-string data-frame out))))
       (otherwise (write-char c out))))))

;; Part2
;
; The second version of the decompression algorithm is like the first,
; only (AxB) markers inside the decompressed data frame are themselves
; decompressed.
;
; Examples:
;
; - (3x3)XYZ becomes XYZXYZXYZ, same as last time.
;
; - X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY which becomes
;   XABCABCABCABCABCABCY.
;
; - (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A
;   repeated 241920 times.
;
; - (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445
;   characters long.
;
; The problem asks us not to do the actual decompression, but simulate
; the process, by counting the number of characters in recursively
; decompressed subsequences.
;;

(defun decompress2 (input-stream)
  "Simulate a recursive decompression of input-stream.

We don't have enough memory to actually decompress the input, so we
simulate it, by counting the number of characters resulted from
recursively decompressing."
  (do ((counter 0)
       (c (read-char input-stream nil) (read-char input-stream nil)))
      ((null c) counter)
    ; Clunky state machine stepper, but it works.
    (case c
      ((#\Space #\Newline) #| Ignore this |#)
      (#\( (let ((decoder (parse-decoder input-stream))
                 (data-frame (new-string)))
             ; Parse decoder
             (with-output-to-string (df-out data-frame)
               ; Read data frame
               (loop for i from 1 to (car decoder) do
                    (write-char (read-char input-stream) df-out)))
             ; Recursively "decompress2" data frame
             (with-input-from-string (in data-frame)
               (let ((times (* (cdr decoder) (decompress2 in))))
                 (incf counter times)))))
      (otherwise (incf counter)))))

;; Tests
(defvar test1.0-strings
  '("ADVENT" "A(1x5)BC" "(3x3)XYZ" "A(2x2)BCD(2x2)EFG"
    "(6x1)(1x3)A" "X(8x2)(3x3)ABCY"))

; Test 1.0
(format t "## Test 1.0:~%")
(dolist (s test1.0-strings)
  (with-input-from-string (in s)
    (let ((decompressed (decompress in)))
      (format t "String ~a decompresses to ~a (length ~d).~%"
              s decompressed (length decompressed)))))

; Test 1.1
(let ((s (apply #'concatenate 'string
                (mapcar #'(lambda (s) (format nil "~a~%" s))
                        test1.0-strings))))
  (format t "## Test 1.1: String~%~s~%" s)
  (with-input-from-string (in s)
    (let ((decompressed (decompress in)))
      (format t "Decompresses to ~a (length ~d)~%"
              decompressed (length decompressed)))))

; Test 1.2
(with-open-file (in "day09-input")
  (with-open-file (out "day09-output"
                       :direction :output
                       :if-exists :supersede)
    (let ((decompressed (decompress in)))
      (write-string decompressed out)
      (format t "## Test 1.2: day09-input decompresses to day09-output")
      (format t " (length ~d).~%" (length decompressed)))))
(format t "~%~%")

; Test 2.0
(defvar test2.0-strings
  '("(3x3)XYZ" "X(8x2)(3x3)ABCY"
    "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"))

(format t "## Test 2.0:~%")
(dolist (s test2.0-strings)
  (with-input-from-string (in s)
    (format t "String ~a decompresses to length ~d.~%"
            s (decompress2 in))))

; Test 2.1
(with-open-file (in "day09-input")
  (format t "## Test 2.1: day09-input decompresses to length ~d."
          (decompress2 in)))
