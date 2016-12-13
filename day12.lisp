;; Advent of Code 2016, Day 12
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; The problem asks us to write a simple interpreter for a simple machine
; with a very simple Instruction Set Arhitecture (ISA).
;
; The machine has four registers, a, b, c and d (initialized to 0) and
; supports four instructions:
;
; - cpy x y copies the register/immediate value x into register y
;
; - inc x increases the value of register x by 1
;
; - dec x decreases the value of register x by 1
;
; - jnz x y jumps relative to the current "instruction pointer" by an
;   immediate offset of y if x (register value or immediate) is not zero
;
; The machine halts when the "instruction pointer" goes out of bounds.
;
; We are asked to tell what is the value of register a when the machine
; halts.
;;

(defstruct mach
  "A simple register machine."
  (reg-a 0 :type integer)
  (reg-b 0 :type integer)
  (reg-c 0 :type integer)
  (reg-d 0 :type integer)
  (reg-ip 0 :type integer))

(defvar reg-strs '(("a" . a) ("b" . b) ("c" . c) ("d" . d)))
(defun reg-to-func (reg)
  (case reg
    (a 'mach-reg-a)
    (b 'mach-reg-b)
    (c 'mach-reg-c)
    (d 'mach-reg-d)))

(defmacro parse-reg (regstr)
  `(cdr (assoc ,regstr reg-strs :test 'equal)))

(defun parse-instr (line)
  (let* ((regspec "a|b|c|d")
         (immspec "(-|\\+)?[0-9]+")
         (cpyspec (format nil "cpy ((~a)|(~a)) (~a)"
                          immspec regspec regspec))
         (incspec (format nil "inc (~a)" regspec))
         (decspec (format nil "dec (~a)" regspec))
         (jnzspec (format nil "jnz ((~a)|(~a)) (~a)"
                          immspec regspec immspec))
         (instspec (format nil "~a|~a|~a|~a"
                           cpyspec incspec decspec jnzspec)))
    (multiple-value-bind (_ arr)
        (cl-ppcre:scan-to-strings instspec line)
      (declare (ignore _))
      (when arr
        (cond
          ((elt arr 1) ; cpy imm reg
           `(cpy ,(parse-integer (elt arr 1))
                 ,(parse-reg (elt arr 4))))
          ((elt arr 3) ; cpy reg reg
           `(cpy ,(parse-reg (elt arr 3))
                 ,(parse-reg (elt arr 4))))
          ((elt arr 5) ; inc reg
           `(inc ,(parse-reg (elt arr 5))))
          ((elt arr 6) ; dec reg
           `(dec ,(parse-reg (elt arr 6))))
          ((elt arr 8) ; jnz imm imm
           `(jnz ,(parse-integer (elt arr 8))
                 ,(parse-integer (elt arr 11))))
          ((elt arr 10) ; jnz reg imm
           `(jnz ,(parse-reg (elt arr 10))
                 ,(parse-integer (elt arr 11)))))))))

; An "instruction memory" is a vector of instructions.
(defun make-instr-mem (input-stream)
  "Make instruction memory."
  (let ((instr-list nil))
    (do ((line (read-line input-stream nil) (read-line input-stream nil)))
        ((null line))
      (push (parse-instr line) instr-list))
    (coerce (reverse instr-list) 'vector)))

(defun is-running (mach instr-mem)
  "Check whether we have halted."
  (let ((max-ip (- (length instr-mem) 1))
        (curr-ip (mach-reg-ip mach)))
    (and (>= curr-ip 0) (<= curr-ip max-ip))))

(defmacro setf-reg (reg mach value)
  `(funcall (fdefinition (list 'setf (reg-to-func ,reg)))
            ,value ,mach))

(defmacro getf-reg (reg mach)
  `(funcall (reg-to-func ,reg) ,mach))

(defun mach-step (mach instr-mem)
  ; First, fetch
  (let ((instr (elt instr-mem (mach-reg-ip mach))))
    ; Increment ip
    (incf (mach-reg-ip mach))
    ; Decode/execute
    (case (car instr)
      (cpy (setf-reg (caddr instr) mach
                     (if (integerp (cadr instr))
                         (cadr instr)
                         (getf-reg (cadr instr) mach))))
      (inc (let ((val (getf-reg (cadr instr) mach)))
             (setf-reg (cadr instr) mach (+ val 1))))
      (dec (let ((val (getf-reg (cadr instr) mach)))
             (setf-reg (cadr instr) mach (- val 1))))
      (jnz (let ((val (if (integerp (cadr instr))
                          (cadr instr)
                          (getf-reg (cadr instr) mach)))
                 (off (- (caddr instr) 1)))
             (when (not (= 0 val))
               (incf (mach-reg-ip mach) off))))))
  mach)

(defun mach-run (mach instr-mem)
  (do ()
      ((not (is-running mach instr-mem)) mach)
    ;(format t "Executing: ~s~%" (elt instr-mem (mach-reg-ip mach)))
    ;(format t "Mach: ~s~%" mach)
    ;(format t "Instr mem: ~s~%" instr-mem)
    ;(read-char)
    (mach-step mach instr-mem)))

;; Tests
(defvar test1.0-input
"cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
")

(let ((instr-mem (with-input-from-string (in test1.0-input)
                   (make-instr-mem in)))
      (mach (make-mach)))
  (format t "## Test 1.0: from test1.0-input.")
  (mach-run mach instr-mem)
  (format t "Mach: ~s~%" mach))

(let ((instr-mem (with-open-file (in "day12-input")
                   (make-instr-mem in)))
      (mach (make-mach)))
  (format t "## Test 1.1: from day12-input.")
  (mach-run mach instr-mem)
  (format t "Mach: ~s~%" mach))

(let ((instr-mem (with-open-file (in "day12-input")
                   (make-instr-mem in)))
      (mach (make-mach)))
  (setf-reg 'c mach 1)
  (format t "## Test 2.1: from day12-input.")
  (mach-run mach instr-mem)
  (format t "Mach: ~s~%" mach))

