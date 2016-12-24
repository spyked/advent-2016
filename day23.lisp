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
         (instspec (format nil "~a|~a|~a|~a|~a"
                           cpyspec incspec decspec jnzspec tglspec)))
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
           `(tgl ,(parse-reg (elt arr 15)))))))))

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
    (tgl (or (symbolp (cadr instr)) (numberp (cadr instr))))))

(defun try-add (mach instr-mem)
  (let* ((ip (mach-reg-ip mach))
         (len (length instr-mem))
         (i1 (elt instr-mem (- ip 1)))
         (i2 (when (< ip len) (elt instr-mem ip)))
         (i3 (when (< (1+ ip) len) (elt instr-mem (1+ ip))))
         (reg1 nil)
         (reg2 nil))
    (when (and i1 i2 i3)
      (cond
        ; inc reg1; dec reg2; jnz reg2 -2
        ((and (eq (car i1) 'inc)
              (eq (car i2) 'dec)
              (eq (car i3) 'jnz)
              (eq (cadr i2) (cadr i3))
              (eq (caddr i3) -2))
         (setq reg1 (cadr i1))
         (setq reg2 (cadr i2)))
        ; dec reg2; inc reg1; jnz reg2 -2
        ((and (eq (car i1) 'dec)
              (eq (car i2) 'inc)
              (eq (car i3) 'jnz)
              (eq (cadr i1) (cadr i3))
              (eq (caddr i3) -2))
         (setq reg1 (cadr i2))
         (setq reg2 (cadr i1))))
      (when (and reg1 reg2)
        ;(format t "opt-add ~s ~s~%" reg1 reg2)
        (setf-reg reg1 mach (+ (getf-reg reg1 mach) (getf-reg reg2 mach)))
        (setf-reg reg2 mach 0)
        (incf (mach-reg-ip mach) 2)
        t))))

(defun try-opt (mach instr-mem)
  (cond
    ; Add
    ((try-add mach instr-mem))))

(defun mach-step (mach instr-mem)
  ; First, fetch
  (let ((instr (elt instr-mem (mach-reg-ip mach))))
    ; Increment ip
    (incf (mach-reg-ip mach))
    ; Decode/execute
    (when (valid-instr? instr)
     (let ((opt (try-opt mach instr-mem)))
      (when (not opt)
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
                               ((dec tgl) (alter-mnem inc old-instr))
                               (jnz (alter-mnem cpy old-instr))
                               (cpy (alter-mnem jnz old-instr)))))))))))))
  mach)

(defun mach-run (mach instr-mem)
  (do ()
      ((not (is-running mach instr-mem)) mach)
    ;; (format t "Executing: ~s~%" (elt instr-mem (mach-reg-ip mach)))
    ;; (format t "Mach: ~s~%" mach)
    ;; (format t "Instr mem: ~s~%" instr-mem)
    ;; (read-char)
    (mach-step mach instr-mem)))

(defun make-mach-7 ()
  (let ((mach (make-mach)))
    (setf-reg 'a mach 7)
    mach))

(defun make-mach-12 ()
  (let ((mach (make-mach)))
    (setf-reg 'a mach 12)
    mach))

;; Idea to optimize the assembunny machine, for Part 2:
;
; The machine is pretty inefficient when given the input for Part 2, but
; given that we run it on a real computer, we can optimize some of the
; operations.
;
; For example, the sequence:
;
; inc a
; dec c
; jnz c -2
;
; is a loop that adds to a, decreases c and repeats while c is not
; zero. This is equivalent to adding c to a, storing the result in a,
; then setting c to zero. So in an extended assembunny language, this
; would be equivalent to:
;
; add c a
; cpy 0 c
;
; So we define the implicit instructions add, sub and nop and we define
; the following transformations:
;
; inc {reg1}         nop
; dec {reg2}     ->  add {reg2} {reg1}
; jnz {reg2} -2      sub {reg2} {reg2}
;
; and
;
; dec {reg2}         nop
; inc {reg1}     ->  add {reg2} {reg1}
; jnz {reg2} -2      sub {reg2} {reg2}
;
; There may be others, such as for multiply instructions (this is given
; as a hint in the problem), but we ignore them for now.
;
; One caveat of this approach is that, given the existence of tgl, we
; cannot perform this optimization statically (because the code is not
; unmodifiable, as assumed on most mainstream systems we know). So we
; need to inspect these patterns *at run-time*, which is both
; interesting and burdensome for the implementation of our CPU.
;;

;; Tests
(let ((instr-mem (with-input-from-string (in test-input)
                   (make-instr-mem in)))
      (mach (make-mach-7)))
  (format t "## Test 0.1: from test-input.~%")
  (mach-run mach instr-mem)
  (format t "Machine state is: ~s,~% ~s~%" mach instr-mem))

(let ((instr-mem (with-open-file (in "day23-input")
                   (make-instr-mem in)))
      (mach (make-mach-7)))
  (format t "## Test 1: from day23-input.~%")
  (mach-run mach instr-mem)
  (format t "Machine state is: ~s,~% ~s~%" mach instr-mem))

(let ((instr-mem (with-open-file (in "day23-input")
                   (make-instr-mem in)))
      (mach (make-mach-12)))
  (format t "## Test 2: from day23-input.~%")
  (mach-run mach instr-mem)
  (format t "Machine state is: ~s,~% ~s~%" mach instr-mem))
