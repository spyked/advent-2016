;; Advent of Code 2016, Day 13

;; Problem description
;
; Maze search time!
;
; We have a 2D maze addressed by (x,y) pairs. Each (x,y) pair may be
; either a wall or open space. x and y are positive and are indefinitely
; high.
;
; The bot can move only in open spaces, and only vertically or
; horizontally. The bot starts at (1,1).
;
; Determining whether a coordinate represents a wall or open space is
; done using the following algorithm:
;
; - compute x*x + 3*x + 2*x*y + y + y*y
;
; - add the office designer's favourite number (the puzzle input)
;
; - using the binary representation of the obtained number, count the
;   number of bits that are 1 b; if b is even, then (x,y) is an open
;   space; otherwise it's a wall.
;
; The problem also gives a target coordinate and it asks for the minimum
; number of steps required to reach that coordinate.
;;

(defun integer->bit-vector (n)
  "Convert an integer to a bit vector."
  (let ((bit-list nil))
    (do ((k n))
        ((= 0 k))
      (multiple-value-bind (i r)
          (truncate k 2)
        (push r bit-list)
        (setq k i)))
    (coerce bit-list 'bit-vector)))

(defun count-one-bits (n)
  "Count the one bits in a given number."
  (let ((ret 0))
    (do ((k n))
        ((= 0 k) ret)
      (multiple-value-bind (i r)
          (truncate k 2)
        (incf ret r)
        (setq k i)))))

(defun coord-type (point input)
  "Found the coordinate type of a point, according to the problem
description."
  (let* ((x (car point))
         (y (cdr point))
         (s0 (+ (* x x) (* 3 x) (* 2 x y) y (* y y)))
         (s1 (+ s0 input)))
    (if (evenp (count-one-bits s1))
        'open-space
        'wall)))

(defun states-from (point input)
  "Return all the (legal) states starting from point."
  (let ((x (car point))
        (y (cdr point)))
    (remove-if-not #'(lambda (p) (and (>= (car p) 0)
                                      (>= (cdr p) 0)
                                      (case (coord-type p input)
                                        (open-space t)
                                        (wall nil))))
                   (mapcar #'(lambda (p)
                               (cons (+ (car p) x) (+ (cdr p) y)))
                           '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))))))

(defun distance-from (p p-dest)
  "Compute the manhattan distance from a dest point to a p."
  (abs (- (+ (car p-dest) (cdr p-dest))
          (+ (car p) (cdr p)))))

(defun sort-states (point-list target-point)
  "Destructively sort a list of points."
  (sort point-list #'(lambda (p1 p2)
                       (< (distance-from p1 target-point)
                          (distance-from p2 target-point)))))

(defun is-discovered (point discovered)
  "Is a state discovered?"
  (assoc point discovered :test 'equal))

(defun explore-bfs (discovered queue final-state &optional (input 0))
  "(Destructively) explore using BFS starting from queue state."
  (do ((is-final nil))
      ((or is-final (null queue)) (cons is-final discovered))
    (let* ((p (pop queue))
           (state (car p)))
      (when (not (is-discovered state discovered))
        (push p discovered)
        (let ((new-states (sort-states (states-from state input) final-state)))
          (loop for new-state in new-states do
               (when (not (is-discovered new-state discovered))
                 (setq queue (nconc queue (list (cons new-state state)))))
               (when (equal new-state final-state)
                 (push (cons new-state state) discovered)
                 (setq is-final t)
                 (return))))))))

(defun find-path (discovered initial final)
  (if (equal initial final)
      (list initial)
      (let* ((p (is-discovered final discovered))
             (parent (cdr p)))
        (cons final (find-path discovered initial parent)))))

(defvar initial-state
  (cons 1 1))

;; Part 2
;
; How many locations are reachable from the starting point (including
; itself) in at most 50 steps?
;
; This is basically determined from a spanning tree of the states.
;;
(defvar part2-steps-limit 50)

(defun all-states-from (point discovered)
  "Given a list of discovered, find all the reachable states from
point."
  (remove-if-not #'(lambda (ps) (equal (cdr ps) point))
                 discovered))

(defun spanning (point discovered &optional (depth 0))
  (if (>= depth part2-steps-limit)
      1
      (1+ (reduce #'+
               (mapcar #'(lambda (ps) (spanning (car ps)
                                                discovered
                                                (1+ depth)))
                       (all-states-from point discovered))))))

;; Tests
(let* ((test1.0-input 10)
       (test1.0-dest (cons 7 4))
       (discovered (explore-bfs nil
                                `((,initial-state . 'none))
                                test1.0-dest
                                test1.0-input))
       (path (find-path (cdr discovered) initial-state test1.0-dest)))
  (format t "## Test 1.0~%Path is ~s~%of length ~d~%"
          path (length path)))

(let* ((test1.1-input 1350)
       (test1.1-dest (cons 31 39))
       (discovered (explore-bfs nil
                                `((,initial-state . 'none))
                                test1.1-dest
                                test1.1-input))
       (path (find-path (cdr discovered) initial-state test1.1-dest)))
  (format t "## Test 1.1~%Path is ~s~%of length ~d~%"
          path (length path))
  (format t "## Test 2.1~%~d states are reachable from ~s~%"
          (spanning initial-state (cdr discovered))
          initial-state))
