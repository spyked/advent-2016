;; Advent of Code 2016, Day 11
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; (The description is long, but the problem is actually simpler to
; describe today, phew!)
;
; This is a twist on a classic state space search problem. We have a
; state space where each state comprises four rooms/floors that can hold
; objects, and an elevator that can move at least one object and at most
; two objects *exactly* one room up or down at a time.
;
; The objects can be generators made of a certain material, or chips
; that are compatible with said material. For every given chip, a state
; is legal only if:
;
; - the chip has its generator counterpart in the same room, in which
;   case any other objects may be present in the room, or
;
; - the chip only has at most other chips in the same room with it (no
;   other generators than its counterpart).
;
; The objective is, given the initial state as input, bring all objects
; to the fourth room/floor. The elevator starts on floor 1.
;
; The input is very explicit, e.g.:
;
; "
; The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
; The second floor contains a hydrogen generator.
; The third floor contains a lithium generator.
; The fourth floor contains nothing relevant.
; "
;;

; A game state is a structure comprising two elements: the current floor
; (initially 1) and an association list whose keys are floors and its
; elements are lists of objects.
(defstruct state
  (floor 1 :type integer)
  (rooms nil :type list))

; We shouldn't bother with this, but let's create a parser that parses a
; state for us.
(defparameter numerals
  '(("first" . 1) ("second" . 2) ("third" . 3) ("fourth" . 4)))
(defun parse-floor (str)
  "Parse a floor statement, e.g. \"The first floor\" -> 1."
  (let ((which (multiple-value-bind (_ arr)
                   (cl-ppcre:scan-to-strings "The ([a-z]+) floor" str)
                 (declare (ignore _))
                 (when arr (elt arr 0)))))
    (when which
      (cdr (assoc which numerals :test 'equal)))))

(defun parse-object (str)
  "Parse an object statement, as given by the problem description."
  (let* ((generator-spec "a ([a-z]+) generator")
         (microchip-spec "a ([a-z]+)-compatible microchip")
         (nothing-spec "(nothing) relevant")
         (spec (format nil "~a|~a|~a"
                       generator-spec microchip-spec nothing-spec))
         (arr (multiple-value-bind (_ arr)
                  (cl-ppcre:scan-to-strings spec str)
                (declare (ignore _)) arr)))
    (when arr
      (cond
        ((elt arr 0) (cons 'generator (elt arr 0)))
        ((elt arr 1) (cons 'microchip (elt arr 1)))
        ((elt arr 2) 'nothing)))))

(defun parse-line (line)
  "Parse an input line."
  (let* ((seps " contains |, | and |\\.")
         (toks (cl-ppcre:split seps line)))
    (when toks
      (let ((floor-str (car toks))
            (objects-str (cdr toks)))
        (cons (parse-floor floor-str)
              ; Remove nothings
              (remove-if #'(lambda (obj) (eq obj 'nothing))
                         (mapcar #'parse-object objects-str)))))))

(defun get-floor (state floor)
  "Get floor from state."
  (assoc floor (state-rooms state)))

(defun update-objs (state floor objs)
  (let ((mem (member floor (state-rooms state) :key #'car)))
   (if mem
       (setf (car mem) (cons floor objs))
       (push (cons floor objs) (state-rooms state)))))

(defmacro add-objects-to-state (state floor new-objs)
  (let ((floor-objs (gensym)))
    `(let ((,floor-objs (get-floor ,state ,floor)))
       (update-objs ,state ,floor
                    (append ,new-objs (cdr ,floor-objs))))))

(defmacro remove-objects-from-state (state floor new-objs)
  (let ((floor-objs (gensym)))
    `(let ((,floor-objs (get-floor ,state ,floor)))
       (update-objs ,state ,floor
                    (set-difference (cdr ,floor-objs)
                                    ,new-objs
                                    :test 'equal)))))

(defun make-initial-state (input-stream)
  "Read input-stream and make initial state."
  (do* ((state (make-state))
        (line (read-line input-stream nil) (read-line input-stream nil))
        (floor (parse-line line) (parse-line line)))
      ((null line) state)
    (when floor
      (add-objects-to-state state (car floor) (cdr floor)))))

;; Utility functions for working with game states and objects.
(defun set-equal (S1 S2)
  "Check whether two sets are equal."
  (let ((ret t))
    (loop for el in S1 do
         (when (not (member el S2 :test 'equal))
           (setq ret nil)
           (return)))
    (when ret
      (loop for el in S2 do
         (when (not (member el S1 :test 'equal))
           (setq ret nil)
           (return))))
    ret))

(defun combs-of (S k &optional (acc nil))
  "Combinations of elements in S in groups of at most k."
  (if (or (null S) (= 0 k))
      (list acc)
      (append (combs-of (remove (car S) S) (- k 1) (cons (car S) acc))
              (combs-of (cdr S) k acc))
      ))

(defun state-copy (state)
  "Copy a state."
  (make-state :floor (state-floor state)
              :rooms (mapcar #'copy-list (state-rooms state))))

(defun final-state (state)
  "Build the final state from an existing one."
  (let ((final (make-state :floor 4
                           :rooms '((1) (2) (3) (4)))))
    (add-objects-to-state
     final 4
     (apply #'concatenate 'list
            (mapcar #'cdr (state-rooms state))))
    final))


(defun is-equal-state (state1 state2)
  "Checks whether a state is final."
  (and (= (state-floor state1) (state-floor state2))
       (let ((rooms1 (state-rooms state1))
             (ok t))
         (loop for i from 1 to (length rooms1) do
              (when (not (set-equal (get-floor state1 i)
                                    (get-floor state2 i)))
                (setq ok nil) (return)))
         ok)))

(defun generators? (room)
  "Are there any generators in the room?"
  (assoc 'generator room))

(defun matching-generator? (type room)
  "Is there a matching generator for type in the room?"
  (member (cons 'generator type) room :test 'equal))

(defun is-legal (state)
  "Checks whether a state is legal."
  (let ((floor (state-floor state))
        (rooms (state-rooms state))
        (rooms-ok t))
    (when (and (> floor 0) (< floor 5))
     (dolist (room rooms)
       (let ((objs (cdr room)))
         (dolist (obj objs)
           (case (car obj)
             (microchip (when (and
                               ; There are generators in the same room,
                               ; but none of our type.
                               (generators? (cdr room))
                               (not (matching-generator? (cdr obj) room)))
                          (setq rooms-ok nil)))))))
     rooms-ok)))

(defun take (n L)
  (if (or (= 0 n) (null L))
      nil
      (cons (car L) (take (- n 1) (cdr L)))))

;; And now, the actual problem...
(defun movable-objs-from (state)
  "Return all the objects that can be moved from a given state."
  (cdr (get-floor state (state-floor state))))

(defun states-from (state)
  "Generate all states starting from state."
  (let ((state-list nil))
    ; We remove moves with 0 objects from combinations of max. 2 of
    ; objects on current floor.
    (dolist (objs (remove nil (combs-of (movable-objs-from state) 2)))
      (let ((new-state-up (state-copy state))
            (new-state-down (state-copy state)))
        ; Remove objects
        (remove-objects-from-state new-state-up (state-floor state) objs)
        (remove-objects-from-state new-state-down (state-floor state) objs)

        ; Update floors
        (incf (state-floor new-state-up))
        (decf (state-floor new-state-down))

        ; Add objects to new states
        (add-objects-to-state new-state-up (state-floor new-state-up) objs)
        (add-objects-to-state new-state-down (state-floor new-state-down) objs)

        ; Push new states
        (push new-state-up state-list)
        (push new-state-down state-list)))
    state-list))

(defun distance-from-final (state)
  (let ((ret 0)
        (multiplier 0))
    (loop for i from 4 downto 0 do
         (incf ret (* multiplier 20 (length (get-floor state i))))
         (incf multiplier))
    ret))

(defun sort-states (state-list)
  (sort state-list #'(lambda (state1 state2)
                       (< (distance-from-final state1)
                          (distance-from-final state2)))))

(defun legal-states-from (state)
  "Generate all legal states starting from state."
  (remove-if-not #'is-legal (states-from state)))

(defun explore-from (state final &optional (acc nil) (depth 0))
  "Explore starting from state.

The input state is assumed to be legal."
  ;(format t "State: ~s~%~s~%######~%" state acc)
  (if (is-equal-state state final)
      (progn
        (format t "AM FINAL~%")
        (list (reverse (cons state acc))))
      (let ((new-states (take 3 (sort-states (legal-states-from state))))
            (new-sols))
        ;(format t "new states: ~s~%#########~%" new-states)
        (loop for new-state in new-states do
             (if (and (not (member new-state acc :test #'is-equal-state))
                        (< depth 25)
                      )
               (let ((sol (explore-from new-state final (cons new-state acc)
                                        (1+ depth))))
                 (when sol (setq new-sols (nconc sol new-sols)) (return)))
               (progn
                 ;(format t "######### Stopped from state:~%~s~%" acc)
                 ;(format t "Depth: ~d~%" depth)
                 )))
        new-sols)))

(defun explore-bfs (discovered queue final-st)
  "(Destructively) explore using BFS starting from queue state."
  (do ((final nil))
      ((or final (null queue)) (cons final discovered))
    (let* ((p (pop queue))
           (state (car p)))
      (when (not (assoc state discovered :test #'is-equal-state))
        (push p discovered)
        (let ((new-states (take 3 (sort-states (legal-states-from state)))))
          (loop for new-state in new-states do
               (when (not (rassoc new-state discovered :test #'is-equal-state))
                 (setq queue (nconc queue (list (cons new-state state)))))
               (when (is-equal-state new-state final-st)
                 (push (cons new-state state) discovered)
                 (setq final t)
                 (return))))))))

(defun find-path (discovered initial final)
  (if (is-equal-state initial final)
      (list initial)
      (let* ((p (assoc final discovered :test #'is-equal-state))
             (parent (cdr p)))
        (format t "PPP : ~s.~%" p)
        (cons final (find-path discovered initial parent)))))

;; Tests
(defvar test1.0-input
  (format nil "~a~%~a~%~a~%~a~%"
          "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
          "The second floor contains a hydrogen generator."
          "The third floor contains a lithium generator."
          "The fourth floor contains nothing relevant."))

(defvar null-state (make-state :floor 0 :rooms nil))

;; Old DFS solution.
;; (with-input-from-string (in test1.0-input)
;;   (let ((s (make-initial-state in)))
;;     (format t "## Test 1.0: from test1.0-input~%")
;;     (format t "Solution has ~d states.~%"
;;             (length (car (explore-from s (final-state s)))))))

;; New BFS solutions
;; (with-input-from-string (in test1.0-input)
;;   (let ((s (make-initial-state in)))
;;     (format t "## Test 1.0: from test1.0-input~%")
;;     (let* ((sol0 (explore-bfs nil (list (cons s null-state)) (final-state s)))
;;            (path (find-path (cdr sol0) s (final-state s))))
;;      (format t "Solution: ~s.~%" sol0)
;;      (format t "Path: ~s.~%### Of length: ~d.~%"
;;              path (length path)))))

;; (with-open-file (in "day11-input")
;;   (let ((s (make-initial-state in)))
;;     (format t "## Test 1.1: from day11-input~%")
;;     (let* ((sol0 (explore-bfs nil (list (cons s null-state)) (final-state s)))
;;            (path (find-path (cdr sol0) s (final-state s))))
;;       (format t "Discovered ~s~%" sol0)
;;       (format t "Path ~s.~%### Of length: ~d.~%"
;;               path (length path)))))

;; (with-open-file (in "day11-input")
;;   (let ((s (make-initial-state in)))
;;     (add-objects-to-state s 1
;;                           '((generator . "elerium") (microchip . "elerium")
;;                             (generator . "dilithium") (microchip . "dilithium")))
;;     (format t "## Test 2.1: from day11-input~%")
;;     (format t "Initial state: ~s~%" s)
;;     (let* ((sol0 (explore-bfs nil (list (cons s null-state)) (final-state s)))
;;            (path (find-path (cdr sol0) s (final-state s))))
;;       (format t "Discovered ~s~%" sol0)
;;       (format t "Path ~s.~%### Of length: ~d.~%"
;;               path (length path)))))
