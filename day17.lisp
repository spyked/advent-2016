;; Advent of Code 2016, Day 17
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :sb-md5)) ; my choice of md5 digest implementation

;; Problem description
;
; Trickeh one! We are in a maze consisting of a 4x4 grid of rooms,
; interconnected through doors. We start at 0x0 and the goal is to get
; to 3x3. A path from 0x0 to 3x3 is represented as a sequence of U(p),
; D(own), L(eft) or R(ight) actions.
;
; The trick is that at any given time the doors may be either open or
; closed and locked, based on an input and the path we have taken so
; far. We take the input, we concatenate it with the path (a sequence of
; U, D, L and R), we hash it using MD5 and we use the *first four
; characters* of the hash to determine whether a door is open or locked:
;
; - each of the four characters denotes a direction: up, down, left and
;   right respectively
;
; - if a character is b, c, d, e, or f then the corresponding door is
;   open; otherwise, it is locked
;
; For example, suppose the input is hijkl. Initially the path is empty,
; so md5(hijkl) returns a hash whose first four characters are ced9, so
; U, D and L are accesible; only, there is no U and L, so we're left
; with D.
;
; Then we hash hijklD, we get f2bc, so ULR are accessible, only L is a
; wall; we can go right, which gives 5745 (so a dead end), or up, which
; gives 528e, so R is accessible from there. But going R gives a
; dead-end again, so the problem has no solution for this input.
;
; A few tests:
;
; - ihgpwlah -> DDRRRD
; - kglvqrro -> DDUDRLRRUDRD
; - ulqzkmiv -> DRURDRUDDLLDLUURRDULRLDUUDDDRR
;
; The problem asks to find the *shortest* such path.
;;

; From day14
(defstruct state
  "A game state."
  (x 0 :type integer)
  (y 0 :type integer)
  (path "" :type string))

(defvar *game-input*)
(defvar reachable-state-chars '(#\b #\c #\d #\e #\f))

;; Basic utilities
(defun get-md5-hash (str)
  "Obtain the MD5 hash of str as a string."
  ; md5sum-string returns an array of bytes, which we turn into a list
  ; and display using hex; then we concatenate the result.
  (apply #'concatenate 'string
         (map 'list #'(lambda (byte)
                        (format nil "~(~2,'0x~)" byte))
                 (sb-md5:md5sum-string str))))

(defmacro is-open-c? (c)
  "Is the door given by an MD5 char open?"
  `(member ,c reachable-state-chars))

(defun md5->open (str)
  "Convert an MD5 string to a list of open doors."
  (mapcar #'cdr
          (remove-if-not #'(lambda (pair) (is-open-c? (car pair)))
                         (mapcar #'cons
                                 (coerce (subseq str 0 4) 'list)
                                 '("U" "D" "L" "R")))))

(defmacro manhattan-dist (x1 y1 x2 y2)
  `(abs (- (+ ,x1 ,y1) (+ ,x2 ,y2))))

;; Primitives to determine the accessible states from a given state.
(defun open-doors-in (state)
  "Return all the open doors in a state."
  (assert *game-input*)
  (assert (stringp *game-input*))
  (md5->open (get-md5-hash
              (concatenate 'string *game-input* (state-path state)))))

(defun door->state (state door)
  "Return the new state given by room connected by door, relative to state."
  (let ((new-state (copy-state state)))
    (case (elt door 0)
      (#\U (decf (state-y new-state)))
      (#\D (incf (state-y new-state)))
      (#\L (decf (state-x new-state)))
      (#\R (incf (state-x new-state))))
    (setf (state-path new-state)
          (concatenate 'string (state-path state) door))
    new-state))

(defun states-from (state)
  "Return all the reachable states from a given state."
  (remove-if-not #'(lambda (state)
                     (let ((x (state-x state))
                           (y (state-y state)))
                       (and (>= x 0) (<= x 3)
                            (>= y 0) (<= y 3))))
                 (mapcar #'(lambda (door) (door->state state door))
                         (open-doors-in state))))

(defun is-final-state? (state)
  "Is state final?"
  (and (= (state-x state) 3) (= (state-y state) 3)))

; We have the basic building blocks to get us started. The trickier
; problem is finding a heuristic to guide us through the maze. We have
; two variables that we want to optimize:
;
; - The manhattan distance from point 3x3; we want to get the *minimum*,
;   which is 0. The maximum is 3*3 = 9.
;
; - The number of open doors; we want to get the *maximum*, which is
;   4. The minimum is 0 (dead ends, which we want to avoid altogether).
;
; Getting a good optimizer is in general a *hard* problem, but we can
; probably get away with using something such as (* distance (- 5
; num_doors)) as a state evaluation function. This, combined with the
; assumption that the state space is pretty sparse (so it would seem
; from a cursory look), should make an informed BFS do pretty well.

(defun score (state)
  "Score heuristic of a state."
  (* (manhattan-dist (state-x state) (state-y state) 3 3)
     (- 5 (length (open-doors-in state)))))

(defun sort-states (state-list &optional (func #'score))
  "Destructively sort state-list according to heuristic func."
  (sort state-list #'(lambda (state1 state2)
                       (and (< (funcall func state1)
                               (funcall func state2))
                            (< (length (state-path state1))
                               (length (state-path state2)))))))

(defun sort-states2 (state-list &optional (func #'score))
  "Destructively sort state-list according to heuristic func."
  (sort state-list #'(lambda (state1 state2)
                       (and (< (funcall func state1)
                               (funcall func state2))
                            (> (length (state-path state1))
                               (length (state-path state2)))))))

; The BFS should be pretty simple, mainly because there is no way to "go
; back" to a previous state (this comes from the fact that the state
; depends on the path).
(defun explore-bfs (discovered queue)
  "(Destructively) explore using BFS starting from queue state."
  (do ((is-final nil))
      ((or is-final (null queue)) (cons is-final discovered))
    (let* ((p (pop queue))
           (state (car p)))
      (push p discovered)
      (let ((new-states (sort-states (states-from state))))
        (loop for new-state in new-states do
             ;(format t "New state: ~s~%parent ~s~%####~%" new-state state)
             (setq queue (nconc queue (list (cons new-state state))))
             (when (is-final-state? new-state)
               (push (cons new-state state) discovered)
               (setq is-final t)
               (return)
               ))))))

; Let's also try a DFS
(defun explore-dfs (discovered stack)
  "(Destructively) explore using DFS starting from stack state."
  (do ((is-final nil))
      ((or is-final (null stack)) discovered)
    (let* ((p (pop stack))
           (state (car p)))
      (let ((new-states (sort-states2 (states-from state))))
        (loop for new-state in new-states do
             (if (is-final-state? new-state)
                 (progn
                   ;; (format t "Final state: ~s~%parent ~s~%####~%"
                   ;;         new-state state)
                   (push (cons new-state state) discovered))
                 (push (cons new-state state) stack)))))))

(defun final-minimum (discovered &optional (func #'<))
  "Find the final state with the minimum path length."
  (car (reduce #'(lambda (acc el)
                   (if (funcall func
                                (length (state-path (car acc)))
                                (length (state-path (car el))))
                       acc el))
               (remove-if-not #'(lambda (p) (is-final-state? (car p)))
                              discovered))))

;; Tests
(defvar initial-state (make-state))
(let ((inputs '("ihgpwlah" "kglvqrro" "ulqzkmiv"))
      (my-input "vwbaicqe")
      (test-counter 0))
  ;; Part 1
  (dolist (input inputs)
    ; Validation tests
    (setq *game-input* input)
    (format t "## Test 1.~d: input = ~a~%" test-counter input)
    (format t "Solution: ~s~%##~%"
            (final-minimum (explore-dfs nil
                                        (list (cons initial-state 'none)))))
    (incf test-counter))
  ; Main test
  (setq *game-input* my-input)
  (format t "## Test 1.~d: my input is ~a~%" test-counter my-input)
  (format t "Solution: ~s~%"
          (final-minimum (explore-dfs nil
                                      (list (cons initial-state 'none)))))

  ;; Part 2
  (setq test-counter 0)
  (dolist (input inputs)
    ; Validation tests
    (setq *game-input* input)
    (format t "## Test 2.~d: input = ~a~%" test-counter input)
    (let* ((sol (final-minimum
                 (explore-dfs nil (list (cons initial-state 'none)))
                 #'>))
           (sol-len (length (state-path sol))))
      (format t "Solution: ~s~%Of length: ~d~%##~%"
              sol sol-len))
    (incf test-counter))
  
    ; Main test
    (setq *game-input* my-input)
    (format t "## Test 2.~d: my input is ~a~%" test-counter my-input)
    (let* ((sol (final-minimum
                 (explore-dfs nil (list (cons initial-state 'none)))
                 #'>))
           (sol-len (length (state-path sol))))
      (format t "Solution: ~s~%Of length: ~d~%##~%"
              sol sol-len)))
