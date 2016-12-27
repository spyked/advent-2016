;; Advent of Code 2016, Day 24

;; Problem description:
;
; We take as input a map which contains the following types of tiles:
;
; - empty spaces, denoted using #\.
; - walls, denoted using #\#
; - target locations, denoted using numeric digits
;
; We have a robot which starts at the location labeled 0. The goal is to
; visit all the target locations using a minimal number of moves.
;
; For example:

(defvar test-input
  "###########
#0.1.....2#
#.#######.#
#4.......3#
###########")

; The robot starts at 0. It takes: 2 steps to 4 + 4 steps to 1 + 6 steps
; to 2 + 2 steps to 3, which makes a total of 14 steps. This is also the
; minimal number of steps (the solution).
;;

;; We keep a single instance of the map, to not pass it around.
(defvar *game-map*)

;; Map parsing and printing
(defun char->elem (c)
  (case c
    (#\. 'space)
    (#\# 'wall)
    (otherwise (- (char-code c) (char-code #\0)))))

(defun elem->char (e)
  (case e
    (space #\.)
    (wall #\#)
    (otherwise (if (numberp e)
                   (code-char (+ e (char-code #\0)))
                   (error "Unknown element ~s~%" e)))))

(defun parse-input (input-stream)
  (let ((elems
         (do ((line (read-line input-stream nil) (read-line input-stream nil))
              (result nil))
             ((null line) (nreverse result))
           (let ((L (coerce line 'list)))
             (push (mapcar #'char->elem L) result)))))
    (make-array (list (length elems) (length (car elems)))
                :initial-contents elems)))

(defmacro get-pos (x y)
  `(aref *game-map* ,x ,y))

;; Game state representation:
;
; A game state is a triple (x y to-visit) where x is the vertical (row)
; coordinate and y is the horizontal (column) coordinate. to-visit
; represents a list of the target locations left to visit.
;;
(defun make-state (x y to-visit)
  "Make a game state."
  (list x y to-visit))

(defun print-state (state)
  (loop for i from 0 to (- (array-dimension *game-map* 0) 1) do
       (loop for j from 0 to (- (array-dimension *game-map* 1) 1) do
            (if (and (= i (car state)) (= j (cadr state)))
                (format t "R")
                (format t "~c" (elem->char (aref *game-map* i j)))))
       (format t "~%")))

(defun make-initial-state ()
  "Assuming *game-map* is initialized, make initial game state."
  (let ((x nil)
        (y nil)
        (to-visit nil))
    (loop for i from 0 to (- (array-dimension *game-map* 0) 1) do
         (loop for j from 0 to (- (array-dimension *game-map* 1) 1) do
              (let ((e (get-pos i j)))
                (when (numberp e)
                  (if (= 0 e)
                      (setq x i y j)
                      (push e to-visit))))))
    (when (and x y to-visit)
      (make-state x y to-visit))))

(defun score (state)
  "Evaluate a state's score."
  (length (caddr state)))

(defun states-from (state)
  "Get all the valid states from state."
  (let ((pos-list (mapcar #'(lambda (p) (cons (+ (car state) (car p))
                                              (+ (cadr state) (cdr p))))
                          '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))))
    (setq pos-list (delete-if-not
                    #'(lambda (pos)
                        (and (>= (car pos) 0) (>= (cdr pos) 0)
                             (< (car pos) (array-dimension *game-map* 0))
                             (< (cdr pos) (array-dimension *game-map* 1))
                             (not (eq 'wall (get-pos (car pos) (cdr pos))))))
                    pos-list))
    (mapcar #'(lambda (pos)
                (let ((to-visit (caddr state))
                      (target (get-pos (car pos) (cdr pos))))
                  (make-state (car pos) (cdr pos)
                              (if (numberp target)
                                  (remove target to-visit)
                                  to-visit))))
            pos-list)))

;; Exploration stuff
(defmacro prio-queue-pop (prio-queue)
  "Pop from a priority queue prio-queue."
  ; We assume the minimum is always in front of the list.
  `(pop ,prio-queue))

(defun prio-queue-push0 (what where)
  (if (or (null where) (< (car what) (caar where)))
      (cons what where)
      (cons (car where) (prio-queue-push0 what (cdr where)))))

(defmacro prio-queue-push (what where)
  "Push to a priority queue."
  `(setq ,where (prio-queue-push0 ,what ,where)))

(defun path-cost (state costs &optional (eq-func #'equal))
  "Evaluate the path cost of a state."
  (let ((found (assoc state costs :test eq-func)))
    (when found (cdr found))))

(defun explore-astar (initial-state h-func move-func &optional (eq-func #'equal))
  "General A* exploration algorithm."
  (do ((prio-queue (list (cons (funcall h-func initial-state) initial-state)))
       (parents (list (cons initial-state 'none)))
       (cost-to (list (cons initial-state 0)))
       (stop nil))
      ((or stop (null prio-queue)) parents)
    (let* ((p (prio-queue-pop prio-queue))
           (state (cdr p)))
      (if (= 0 (funcall h-func state))
          (setq stop t)
          (let ((cost (1+ (path-cost state cost-to eq-func))))
           (loop for new-state in (funcall move-func state) do
                (let ((new-cost (path-cost new-state cost-to eq-func)))
                  (when (or (not new-cost) (< cost new-cost))
                    (prio-queue-push (cons (+ (funcall h-func new-state) cost)
                                           new-state)
                                     prio-queue)
                    (push (cons new-state cost) cost-to)
                    (push (cons new-state state) parents)))))))))

(defun find-final-state (discovered)
  "Find the final state in discovered."
  (let ((edge (find-if #'(lambda (pair)
                           (null (caddr (car pair))))
                       discovered)))
    (when edge
      (car edge))))

(defun find-path (initial final discovered)
  "Find a path from initial to final in discovered."
  (if (equal initial final)
      (list initial)
      (cons final
            (let ((new-final (cdr (assoc final discovered :test 'equal))))
              (find-path initial new-final discovered)))))

(defun solve-robot (initial-state)
  (let* ((discovered (explore-astar initial-state
                                    #'score
                                    #'states-from))
         (final-state (find-final-state discovered)))
    (find-path initial-state final-state discovered)))

;; Part 2
;
; After visiting all the target locations, the robot needs to return to
; target location labeled 0. This time we're asked for the minimal
; number of steps required to go through all the locations (which we
; know) and then back to location 0.
;;

;; Note:
;
; We apply the same strategy, only with a different score (the number of
; to-visit plus the manhattan distance to label 0) and a different
; find-final-state routine.
;;
(defvar *return-pos*)

(defun score2 (state)
  "Number of to-visit location and manhattan distance to *return-pos*."
  (+ (length (caddr state))
     (abs (- (car state) (car *return-pos*)))
     (abs (- (cadr state) (cdr *return-pos*)))))

(defun find-final-state2 (discovered)
  "Find the final state in discovered."
  (let ((edge (find-if #'(lambda (pair)
                           (and (null (caddr (car pair)))
                                (= 0 (score2 (car pair)))))
                       discovered)))
    (when edge
      (car edge))))

(defun solve-robot2 (initial-state)
  (let* ((discovered (explore-astar initial-state
                                    #'score2
                                    #'states-from))
         (final-state (find-final-state2 discovered)))
    (find-path initial-state final-state discovered)))

;; Tests
(defvar *sol1*)
(defvar *sol2*)
; zero
(format t "## Test 0.1: from test-input~%")
(setq *game-map* (with-input-from-string (in test-input)
                   (parse-input in)))
(let ((solution (solve-robot (make-initial-state))))
  (setq *sol1* (- (length solution) 1))
  (format t "Found solution of length ~d.~%" *sol1*))

(format t "## Test 0.2: from test-input~%")
(setq *return-pos* (let ((initial-state (make-initial-state)))
                      (cons (car initial-state)
                            (cadr initial-state))))
(let ((solution (solve-robot2 (make-initial-state))))
  (setq *sol2* (- (length solution) 1))
  (format t "Found solution of length ~d.~%" *sol2*))

; one
(format t "## Test 1: from day24-input~%")
(setq *game-map* (with-open-file (in "day24-input")
                   (parse-input in)))
(let ((solution (solve-robot (make-initial-state))))
  (setq *sol1* solution)
  (format t "Found solution of length ~d.~%" (- (length *sol1*) 1)))

(format t "## Test 2: from day24-input~%")
(setq *return-pos* (let ((initial-state (make-initial-state)))
                      (cons (car initial-state)
                            (cadr initial-state))))
(let ((solution (solve-robot2 (make-initial-state))))
  (setq *sol2* solution)
  (format t "Found solution of length ~d.~%" (- (length  *sol2*) 1)))
