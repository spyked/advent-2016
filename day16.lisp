;; Advent of Code 2016, Day 16

;; Problem description
;
; The algorithm described in the problem has two steps:
;
; 1. filling a bit string of length n by generating a dragon curve
;    fractal
;
; 2. checksumming the generated bit string using a simple recursive
;    checksum algorithm
;
; Let's take it step by step:
;
; 1. Dragon curves:
;
; We start from an input bit string a. A dragon curve iteration takes a
; copy b of the string, reverses it, flips all the bits, and computes
; the concatenation a0b. For example:
;
; - 1 -> 100
; - 0 -> 001
; - 1011 -> 101100010
; - 111100001010 -> 1111000010100101011110000
;
; We need to iteratively generate dragon curve sequences starting from
; the input, until we get to a sequence of length >= n, which we
; truncate to n.
;
; 2. Checksum:
;
; We start from the length-n sequence. First, we assume n is even. Then,
; we take non-overlapping pairs of bits and we apply the following
; rules:
;
; - 00|11 -> 1
; - 01|10 -> 0
;
; We apply these rules iteratively, until the resulting sequence has an
; odd length.
;
; For example, we take the sequence 110010110100, of length 12. Then:
;
; - 11 -> 1; 00 -> 1; 10 -> 0; 11 -> 1; 01 -> 0; 00 -> 1; so after the
;   first iteration, the result is 110101, of length 6
;
; - 11 -> 1; 01 -> 0; 01 -> 0; after the second iteration, the result is
;   100, of length 3; 3 is odd, so we stop.
;
; To sum steps 1 and 2 together, let's say the input was 10000 and we
; want to fill a string of length 20, then checksum it.
;
; - (< (len 10000) 20), so continue
;
; - 10000 -> 10000011110, and (len 10000011110) is 11
;
; - 10000011110010000111110, and (len 10000011110010000111110) is 23;
;
; - we truncate to 20, resulting in 10000011110010000111
;
; - 10000011110010000111 -> 0111110101 (of length 10)
;
; - 0111110101 -> 01100 (of length 5)
;
; So the result is the checksum 01100.
;
; The problem asks us to fill a disk of length 272 and give the
; resulting checksum.
;;

(defun string->bit-vector (str)
  "Convert a string to a bit vector."
  (let ((bit-list
         (loop for c across str
            collect (case c
                      (#\0 0)
                      (#\1 1)))))
    (coerce bit-list 'bit-vector)))

(defun dragon-curve-step (bitvec)
  "Generate a Dragon Curve iteration of bitvec."
  (concatenate 'bit-vector bitvec #*0
               (reverse (bit-not bitvec))))

(defun dragon-curve-until (bitvec n)
  "Iterate Dragon Curve starting from bitvec until the resulting vector
has a length equal to or greater than n."
  (do ((v bitvec (dragon-curve-step v)))
      ((>= (length v) n) v)))

(defun truncate-bit-vector (bitvec n)
  "Truncates bitvec to length n."
  (subseq bitvec 0 n))

(defun checksum-step (bitvec)
  "Generate a checksum step of bitvec.

Assumes the length of bitvec is even."
  (let ((bit-list
         (loop for i from 0 to (floor (/ (- (length bitvec) 1) 2))
            collect (if (= (bit bitvec (* 2 i))
                           (bit bitvec (1+ (* 2 i))))
                        1 0))))
    (coerce bit-list 'bit-vector)))

(defun checksum (bitvec)
  "Generate a full checksum of bitvec."
  (do ((v bitvec))
      ((oddp (length v)) v)
    (setq v (checksum-step v))))

(defun go-bibi (bitvec n)
  "Solve problem."
  (let* ((dragon (dragon-curve-until bitvec n))
         (trunc (truncate-bit-vector dragon n)))
    (checksum trunc)))

;; Tests
(defvar test1.0-input (string->bit-vector "10000"))
(format t "## Test 1.0: ~s~%" test1.0-input)
(format t "Answer is: ~s~%" (go-bibi test1.0-input 20))

(defvar problem-input (string->bit-vector "01111010110010011"))
(format t "## Test 1.1: ~s~%" problem-input)
(format t "Answer is: ~s~%" (go-bibi problem-input 272))

(format t "## Test 2.1: ~s~%" problem-input)
(format t "Answer is: ~s~%" (go-bibi problem-input 35651584))
