;; Advent of Code 2016, Day 23
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; Asesembunny redux! The input we get is the same as the one from day 12
; (it will be copy-pasted here), with an additional instruction, that
; modifies the code at run-time!
;
; tgl x toggles the instruction x away, i.e.:
;
; - for one-argument instructions, inc becomes dec and all other
;   one-argument instructions become inc.
;
; - for two-argument instructions, jnz becomes cpy and all other
;   two-argument instructions become jnz.
;
; - the arguments of toggled instructions remain identical.
;
; - toggling non-existent instructions (outside the program) does
;   nothing.
;
; - invalid instructions are skipped
;
; - if tgl toggles itself (e.g. tgl a, and a is 0), the instruction is
;   executed only the next time it is reached.
;
; Register a is initialized to 7. We are asked to provide the value of a
; at the end of the program run.
;
; For example, given the following program:

(defvar test-input
  "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a
")

; - cpy 2 a: initializes a to 2
; - tgl a: toggles the third tgl a to inc a
; - tgl a: toggles cpy 1 a to jnz 1 a
; - inc a: incremends a to 3
; - jnz 1 a: jumps 3 instructions away and halts.
;
; At the end, a is 3.
;;
