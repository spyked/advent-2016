;; Advent of Code 2016, Day 25
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description
;
; Assembunny, part 3! We have the same instruction set, plus one extra
; instruction:
;
; - out x: outputs x (immediate or register) to a so-called "clock
;   signal"
;
; The goal is to get out x to output a real clock signal, i.e. a
; sequence of alternating 0 an 1.
;
; The problem is what is the lowest positive integer used to initialize
; register a that causes the code (given as input) to output a clock
; signal of 0, 1, 0, 1, ... ad infinitum.
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
         (jnzspec (format nil "jnz ((~a)|(~a)) ((~a)|(~a))"
                          immspec regspec immspec regspec))
         (tglspec (format nil "tgl (~a)" regspec))
         (outspec (format nil "out ((~a)|(~a))"
                          immspec regspec))
         (instspec (format nil "~a|~a|~a|~a|~a|~a"
                           cpyspec incspec decspec jnzspec
                           tglspec outspec)))
    (multiple-value-bind (_ arr)
        (cl-ppcre:scan-to-strings instspec line)
      (declare (ignore _))
      (when arr
        ;(print arr)
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
          ((elt arr 7) ; jnz
           (let ((operand-1
                  (cond
                    ((elt arr 8) (parse-integer (elt arr 8)))
                    ((elt arr 10) (parse-reg (elt arr 10)))))
                 (operand-2
                  (cond
                    ((elt arr 12) (parse-integer (elt arr 12)))
                    ((elt arr 14) (parse-reg (elt arr 14))))))
             `(jnz ,operand-1 ,operand-2)))
          ((elt arr 15) ; tgl reg
           `(tgl ,(parse-reg (elt arr 15))))
          ((elt arr 17) ; out imm
           `(out ,(parse-integer (elt arr 17))))
          ((elt arr 19) ; out reg
           `(out ,(parse-reg (elt arr 19)))))))))

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

(defmacro alter-mnem (new-mnem instr)
  `(cons ',new-mnem (cdr ,instr)))

(defun valid-instr? (instr)
  "Is a given instruction valid?"
  (case (car instr)
    (cpy (and (or (symbolp (cadr instr)) (numberp (cadr instr)))
              (symbolp (caddr instr))))
    ((inc dec) (symbolp (cadr instr)))
    (jnz (and (or (symbolp (cadr instr)) (numberp (cadr instr)))
              (or (symbolp (caddr instr)) (numberp (caddr instr)))))
    (tgl (or (symbolp (cadr instr)) (numberp (cadr instr))))
    (out (or (symbolp (cadr instr)) (numberp (cadr instr))))))

(defun mach-step (mach instr-mem out-port)
  ; First, fetch
  (let ((instr (elt instr-mem (mach-reg-ip mach))))
    ; Increment ip
    (incf (mach-reg-ip mach))
    ; Decode/execute
    (when (valid-instr? instr)
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
                  (off (- (if (integerp (caddr instr))
                              (caddr instr)
                              (getf-reg (caddr instr) mach))
                          1)))
              (when (not (= 0 val))
                (incf (mach-reg-ip mach) off))))
       (tgl (let* ((val (getf-reg (cadr instr) mach))
                   (ip (+ (mach-reg-ip mach) val -1)))
              (when (and (>= ip 0) (< ip (length instr-mem)))
                (let ((old-instr (elt instr-mem ip)))
                  (setf (elt instr-mem ip)
                        (case (car old-instr)
                          (inc (alter-mnem dec old-instr))
                          ((dec tgl out) (alter-mnem inc old-instr))
                          (jnz (alter-mnem cpy old-instr))
                          (cpy (alter-mnem jnz old-instr))))))))
       (out (let* ((val (if (integerp (cadr instr))
                            (cadr instr)
                            (getf-reg (cadr instr) mach))))
              (push val (cdr out-port)))))))
  mach)

(defun mach-run (mach instr-mem &optional (stop-cond nil))
  (do ((out-port (cons 'sentinel nil)))
      ((or (not (is-running mach instr-mem))
           (and stop-cond (funcall stop-cond)))
       (cons mach (reverse out-port)))
    ;; (format t "Executing: ~s~%" (elt instr-mem (mach-reg-ip mach)))
    ;; (format t "Mach: ~s~%" mach)
    ;; (format t "Instr mem: ~s~%" instr-mem)
    ;; (read-char)
    (mach-step mach instr-mem out-port)))

(defun make-mach-n (n)
  "Make a machine with register a initialized to n."
  (let ((mach (make-mach)))
    (setf-reg 'a mach n)
    mach))

(defun is-one-zero (sig)
  (or (eq 0 sig) (eq 1 sig)))

(defun is-clock-signal (out-port)
  (when (not (or (null out-port)
                 (eq (car out-port) 'sentinel)))
    (let ((sig1 (car out-port)))
      (do* ((op (cdr out-port) (cdr op))
            (sig2 (car op) (car op))
            (so-far t))
           ((or (not so-far) (eq sig2 'sentinel)) so-far)
        (if (or (eq sig1 sig2)
                (not (is-one-zero sig1))
                (not (is-one-zero sig2)))
            (setq so-far nil)
            (progn (setq sig1 sig2)))))))

(defvar *counter*)
(defparameter *counter-max* 500000)
(defun counter-check ()
  (incf *counter*)
  (when (>= *counter* *counter-max*)
    (let ((ret *counter*))
     (setq *counter* 0)
     ret)))

(defun mach-search-n (instr-mem)
  "Search for n that outputs to out-port."
  (setq *counter* 0)
  (do* ((n 0)
        (mach (make-mach-n n) (make-mach-n n))
        (stop nil))
       ((not (null stop)) (values n stop))
    (let ((run (mach-run mach instr-mem #'counter-check)))
      (format t "n = ~d,~%  run = ~s~%" n run)
      ;(read-char)
      (if (is-clock-signal (cdr run))
          (setq stop (cdr run))
          (incf n)))))

;; "Tests"
(defvar puzzle-input
  "cpy a d
cpy 4 c
cpy 633 b
inc d
dec b
jnz b -2
dec c
jnz c -5
cpy d a
jnz 0 0
cpy a b
cpy 0 a
cpy 2 c
jnz b 2
jnz 1 6
dec b
dec c
jnz c -4
inc a
jnz 1 -7
cpy 2 b
jnz c 2
jnz 1 4
dec b
dec c
jnz 1 -4
jnz 0 0
out b
jnz a -19
jnz 1 -21
")

(defvar my-input
  "dec a
dec a
out a
jnz a 3
inc a
jnz 1 -3
dec a
jnz 1 -5
")

; Main test
(let* ((instr-mem (with-input-from-string (in puzzle-input)
                    (make-instr-mem in))))
  (format t "## Test 1: from puzzle-input")
  (multiple-value-bind (n out-port)
      (mach-search-n instr-mem)
    (format t "n = ~d~%" n)
    (format t "out-port = ~s~%" out-port)))
