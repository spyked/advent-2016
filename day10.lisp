;; Advent of Code 2016, Day 10
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; We wish to simulate a system made up of inputs, bots and chips of
; various values. We get the instructions of where each chip initially
; goes and how the bot redistributes them (which we can call a plan) as
; input.
;
; The main rule is that a bot forwards chips only once it has two of
; them. At the end of the simulation, each output should in principle
; contain a number of chips.
;
; An example of input:
;
; value 5 goes to bot 2
; bot 2 gives low to bot 1 and high to bot 0
; value 3 goes to bot 1
; bot 1 gives low to output 1 and high to bot 0
; bot 0 gives low to output 2 and high to output 0
; value 2 goes to bot 2
;
; That means that the initial configuration is:
;
; - bot 0 has no chips
; - bot 1 has value 3
; - bot 2 has chips of (low) value 2 and (high) value 5
;
; - outputs 0,1,2: empty
;
; Then instructions are followed, depending on possibilities. At first,
; only bot 2 can hand chips, so it gives chip 2 to bot 1 and chup 5 to
; bot 0. So the state changes to:
;
; - bot 0: value 5
; - bot 1: value 2 and value 3
; - bot 2: empty
;
; - outputs 0,1,2: empty
;
; Then bot 1 can forward chips, so the state changes to:
;
; - bot 0: value 3 and value 5
; - bot 1: empty
; - bot 2: empty
;
; - output 0: empty
; - output 1: value 2
; - output 2: empty
;
; Finally, bot 0 forwards the chips to outputs:
;
; - bots 0, 1, 2: empty
;
; - output 0: value 5
; - output 1: value 2
; - output 2: value 3
;
; For the given input, the problem asks what is the number of the bot
; that is responsible for handing out chips of values 61 and 17.
;;

; This is a typical planning problem, albeit an easy one, if we assume
; that we have no cycles in the chip distribution graph. Assuming all
; chips eventually reach an output, then we have a DAG through which we
; must simulate the flow of chips.

(defmacro parse-dest (bot-val output-val)
  "Parse destination values as given by parse-instruction.

If bot-val is non-NIL then we return a (:bot n), else we return
a (:output n)."
  `(cond
     (,bot-val `(:bot ,(parse-integer ,bot-val)))
     (,output-val `(:output ,(parse-integer ,output-val)))))

(defun parse-instruction (line)
  "Parse instruction according to problem description."
  ;; BOT ::= bot ([0-9]+)
  ;; OUTPUT ::= output ([0-9]+)
  ;; DEST ::= (BOT|OUTPUT)
  ;; VALUE ::= value ([0-9]+)
  ;; INIT ::= VALUE goes to DEST
  ;; GIVE ::= BOT gives low to DEST and high to DEST
  ;; INSTR ::= INIT|GIVE
  (let* ((bot-spec "bot ([0-9]+)")
         (out-spec "output ([0-9]+)")
         (val-spec "value ([0-9]+)")
         (dest-spec (format nil "(~a|~a)"
                            bot-spec out-spec))
         (init-spec (format nil "~a goes to ~a"
                            val-spec dest-spec))
         (give-spec (format nil "~a gives low to ~a and high to ~a"
                            bot-spec dest-spec dest-spec))
         (instr-spec (format nil "~a|~a"
                             init-spec give-spec)))
    (multiple-value-bind (_ vec)
        (cl-ppcre:scan-to-strings instr-spec line)
      (declare (ignore _))
      (when vec
       (cond
         ; value x goes to (bot y|output y)
         ((elt vec 0) (list 'init
                            :value (parse-integer (elt vec 0))
                            :dest
                            (parse-dest (elt vec 2) (elt vec 3))))
         ; bot x gives low to (bot y|output y) and high to (bot z|output z)
         ((elt vec 4) (list 'give
                            :bot (parse-integer (elt vec 4))
                            :dest-low
                            (parse-dest (elt vec 6) (elt vec 7))
                            :dest-high
                            (parse-dest (elt vec 9) (elt vec 10)))))))))

(defun parse-instruction-list (input-stream)
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (instrs nil))
      ((null line) instrs)
    (push (parse-instruction line) instrs)))

(defstruct (bot
             (:constructor make-bot (id)))
  "A tireless bot."
  id
  (chip-0 nil)
  (chip-1 nil))

(defstruct (output
             (:constructor make-output (id)))
  "An output."
  id
  (chips nil))

(defun equal-bots (b1 b2)
  "Test whether two bots are equal by ID."
  (= (bot-id b1) (bot-id b2)))

(defun equal-outputs (o1 o2)
  "Test whether two bots are equal by ID."
  (= (output-id o1) (output-id o2)))

(defun give-bot (b value)
  "Try to give b a value. If it succeeds, return b."
  (format t "Giving bot ~s value ~d~%" (bot-id b) value)
  (cond
    ((not (bot-chip-0 b)) (setf (bot-chip-0 b) value) b)
    ((not (bot-chip-1 b)) (setf (bot-chip-1 b) value) b)))

(defun give-output (o value)
  "Give a value to output o."
  (format t "Giving output ~s value ~d~%" (output-id o) value)
  (setf (output-chips o) (cons value (output-chips o))))

(defun take-from-bot (b)
  (format t "Taking from bot ~d(~d,~d)~%"
          (bot-id b) (bot-chip-0 b) (bot-chip-1 b))
  (let* ((c0 (bot-chip-0 b))
         (c1 (bot-chip-1 b)))
    (when (and c0 c1)
      (setf (bot-chip-0 b) nil)
      (setf (bot-chip-1 b) nil)
      (if (< c0 c1)
          `(:low ,c0 :high ,c1)
          `(:low ,c1 :high ,c0)))))

(defun can-take-from-bot (b)
  "Test whether b can give both its chips."
  (and (bot-chip-0 b) (bot-chip-1 b)))

(defun can-give-bot (b)
  "Test whether a bot can receive a value."
  (not (can-take-from-bot b)))

(defun add-bot (id bots)
  "Add bot specified by instr to bots."
  (adjoin (make-bot id) bots :key #'bot-id))

(defun add-output (id outputs)
  "Add bot specified by instr to bots."
  (adjoin (make-output id) outputs :key #'output-id))

(defmacro add-bot-or-output (spec bots outputs)
  `(case (car ,spec)
     (:bot (setq ,bots (add-bot (getf ,spec :bot) ,bots)))
     (:output (setq ,outputs (add-output (getf ,spec :output) ,outputs)))))

(defun gather-bots-chips-and-outputs (instr-list)
  "Gather bots, chips and outputs."
  (let ((bots nil)
        (chips nil)
        (outputs nil))
    (dolist (instr instr-list)
      ;(format t "~s~%" instr)
      (case (car instr)
        (init
         (push (getf (cdr instr) :value) chips)
         (add-bot-or-output (getf (cdr instr) :dest) bots outputs))
        (give
         (destructuring-bind (kb bot-id kdl dest-low kdh dest-high)
             (cdr instr)
           (declare (ignore kb kdl kdh))
           (setq bots (add-bot bot-id bots))
           (add-bot-or-output dest-low bots outputs)
           (add-bot-or-output dest-high bots outputs)))))
    (list bots chips outputs)))

(defmacro push-to-end (obj place)
  "Push an object to the end of a list."
  `(nconc ,place (list ,obj)))

(defun all-chips-in-outputs (chips outputs)
  "Verify whether all the chips are placed in outputs."
  (let ((all-outputs (apply #'concatenate 'list
                            (mapcar #'output-chips outputs)))
        (ret t))
    (loop for c in chips do
         (when (not (member c all-outputs))
           (setq ret nil)))
    ret))

(defun can-give-dests (destspec1 destspec2 bots)
  "Check whether we can give chips to two dests."
  (let* ((bot-id1 (getf destspec1 :bot))
         (bot-id2 (getf destspec2 :bot))
         (can-give-bot-id1 (if bot-id1
                               (can-give-bot (find bot-id1 bots :key #'bot-id))
                               t))
         (can-give-bot-id2 (if bot-id2
                               (can-give-bot (find bot-id2 bots :key #'bot-id))
                               t)))
    (and can-give-bot-id1 can-give-bot-id2)))

(defun init (value destspec bots outputs)
  "Execute an init instruction."
  (destructuring-bind (key id) destspec
    (case key
      (:bot (let ((bot (find id bots :key #'bot-id)))
              (when (can-give-bot bot)
                (give-bot bot value))))
      (:output (let ((output (find id outputs :key #'output-id)))
                 (give-output output value))))))

(defmacro give-bot-or-output (value spec bots outputs)
  `(case (car ,spec)
     (:bot (give-bot (find (getf ,spec :bot) ,bots :key #'bot-id) ,value))
     (:output (give-output (find (getf ,spec :output) ,outputs :key #'output-id)
                           ,value))))

(defun give (src-id destspec-low destspec-high bots outputs)
  "Execute a give instruction."
  (let* ((src-bot (find src-id bots :key #'bot-id))
         (can-take (can-take-from-bot src-bot))
         (can-give (can-give-dests destspec-low destspec-high bots)))
    (when (and can-take can-give)
        (let ((takespec (take-from-bot src-bot)))
          (give-bot-or-output (getf takespec :low) destspec-low
                              bots outputs)
          (give-bot-or-output (getf takespec :high) destspec-high
                              bots outputs))
        t)))

(defun go-bibi (input-stream)
  "Solve the problem."
  (let ((instructions (parse-instruction-list input-stream)))
    (destructuring-bind (bots chips outputs)
        (gather-bots-chips-and-outputs instructions)
      (do ((instr (pop instructions) (pop instructions)))
          ((or (null instructions) (all-chips-in-outputs chips outputs)))
        (let ((props (cdr instr)))
         (case (car instr)
           (init (let ((res (init (getf props :value) (getf props :dest)
                                  bots outputs)))
                   (when (not res) (push-to-end instr instructions))))
           (give (give (getf props :bot)
                       (getf props :dest-low) (getf props :dest-high)
                       bots outputs)
                 (push-to-end instr instructions)))))
      (format t "At the end of the day:~%outputs: ~s~%" outputs))))

;; Tests
(defvar test1.0-input
  (format nil "~a~%~a~%~a~%~a~%~a~%~a~%"
          "value 5 goes to bot 2"
          "bot 2 gives low to bot 1 and high to bot 0"
          "value 3 goes to bot 1"
          "bot 1 gives low to output 1 and high to bot 0"
          "bot 0 gives low to output 2 and high to output 0"
          "value 2 goes to bot 2"))
(with-input-from-string (in test1.0-input)
  (format t "## Test 1.0: from test1.0-input~%")
  (go-bibi in))

(with-open-file (in "day10-input")
  (format t "## Test 1.1: from day10-input~%")
  (go-bibi in))
