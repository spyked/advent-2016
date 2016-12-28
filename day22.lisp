;; Advent of Code 2016, Day 22
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; We have a grid (literally grid, as in, this is the network topology)
; of computing nodes, whose storages are labeled /dev/grid/node-xi-yj,
; where i and j are numbers.
;
; We only have direct access to node-x0-y0, but we can df, getting the
; disk usage on all nodes -- this is the puzzle input.
;
; We can also instruct a node to send all its data to an adjacent node,
; which works only if the destination node has enough space on its disk.
;
; The problem asks us to count the number of *viable pairs* of nodes in
; the network. A pair (A, B) of nodes is viable iff:
;
; - Node A is not empty (it has some data on it)
; - A and B are not the same node
; - B has enough free space to fit the data of A on it
;
; How many viable pairs are there?
;;

(defun parse-df-line (line)
  "Parse a line spat by df -h.

We assume all the devices are of the form /dev/grid/node-xi-yj, so we
return a list of the form (i j size used), where size and used represent
the disk size and the number of used terabytes. We ignore redundant
data (avail and use%)."
  (let* ((numspec "([0-9]+)")
         (spec (format nil "/dev/grid/node-x~a-y~a +~aT +~aT +~aT +~a%"
                       numspec numspec numspec numspec numspec numspec)))
    (multiple-value-bind (_ arr)
        (cl-ppcre:scan-to-strings spec line)
      (declare (ignore _))
      (when arr
        ; Make sure our input is correct.
        (assert (= (parse-integer (elt arr 2))
                   (+ (parse-integer (elt arr 3))
                      (parse-integer (elt arr 4)))))
        (list (parse-integer (elt arr 0))
              (parse-integer (elt arr 1))
              (parse-integer (elt arr 2))
              (parse-integer (elt arr 3)))))))

(defun parse-input (input-stream)
  "Parse an input stream according to the reference input."
  (read-line input-stream nil) ; Ignore command
  (read-line input-stream nil) ; Ignore header
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (result nil))
      ((null line) result)
    (push (parse-df-line line) result)))

(defun find-node (node nodes)
  (find node nodes :test
        #'(lambda (x1 x2)
            (and (= (car x1) (car x2))
                 (= (cadr x1) (cadr x2))))))

(defun get-adjacent-nodes (node nodes)
  "Get adjacent nodes for a given node."
  (remove-if #'null
             (mapcar #'(lambda (node offset)
                         (find-node (list (+ (car node) (car offset))
                                          (+ (cadr node) (cdr offset))
                                          0 0) nodes))
                     (list node node node node)
                     (list '(0 . 1) '(1 . 0) '(0 . -1) '(-1 . 0)))))

(defun adjacent-node-pairs (nodes)
  "Get all pairs of adjacent nodes."
  (apply #'concatenate 'list
         (mapcar #'(lambda (node)
                     (mapcar #'(lambda (neighbour)
                                 (cons node neighbour))
                             (get-adjacent-nodes node nodes)))
                 nodes)))

(defun node-pairs (nodes)
  "Get all pairs of nodes."
  (apply #'concatenate 'list
         (mapcar #'(lambda (node)
                     (mapcar #'(lambda (other-node)
                                 (cons node other-node))
                             nodes))
                 nodes)))

(defun is-viable? (node-pair)
  "Is a pair (node1 . node2) viable?"
  (let ((node1 (car node-pair))
        (node2 (cdr node-pair)))
    (and (> (cadddr node1) 0)
         (not (and (= (car node1) (car node2))
                   (= (cadr node1) (cadr node2))))
         (<= (cadddr node1) (- (caddr node2) (cadddr node2))))))

(defun viable-pairs (nodes)
  "Return all the viable pairs in a list of nodes."
  (remove-if-not #'is-viable? (node-pairs nodes)))

(defun viable-adjacent-pairs (nodes)
  "Return all the viable pairs in a list of nodes."
  (remove-if-not #'is-viable? (adjacent-node-pairs nodes)))

(defun viable-neighbouring-pairs (node nodes)
  "Find neighbours of node that can store data in it. Return pairs."
  (remove-if-not #'(lambda (pair)
                     (let ((node1 (car pair))
                           (node2 (cdr pair)))
                       ; Assumes node2 is empty!
                       (< (cadddr node1) (caddr node2))))
                 (mapcar #'(lambda (node1)
                             (cons node1 node))
                         (get-adjacent-nodes node nodes))))

;; Part 2
;
; Again, an optimization problem. We want to move the data from
; node-xn-y0 (where n is the highest index there is) to node-x0-y0. For
; this, we need to move data around. We're given a test example for
; which n = 2.
;;

; State representation and manipulation
(defstruct (state (:constructor mk-state0 (&key data-pos empty-pos nodes)))
  data-pos empty-pos nodes)

(defun max-x (nodes)
  "Get the maximum x from a list of nodes."
  (reduce #'(lambda (acc node)
              (max acc (car node)))
          nodes :initial-value 0))

(defun find-empty-pos (nodes)
  (let ((node (find-if #'(lambda (node)
                           ; Used is 0
                           (= 0 (cadddr node)))
                       nodes)))
    (cons (car node) (cadr node))))

(defun make-state (nodes)
  "Make a fresh initial state from nodes."
  (mk-state0 :data-pos (cons (max-x nodes) 0)
             :empty-pos (find-empty-pos nodes)
             :nodes nodes))

(defmacro find-node-pos (pos nodes)
  "Find a node based on a pos."
  `(find-node (list (car ,pos) (cdr ,pos) 0 0) ,nodes))

(defun move-data (state old-data-pos new-data-pos)
  "Move data in state to new-data-pos."
  (let* ((data-pos (state-data-pos state))
         (nodes (state-nodes state))
         (src-node (find-node-pos old-data-pos nodes))
         (dest-node (find-node-pos new-data-pos nodes))
         (new-src-node (list (car src-node) (cadr src-node)
                             (caddr src-node) 0))
         (new-dest-node (list (car dest-node) (cadr dest-node)
                                        (caddr dest-node)
                                        (+ (cadddr dest-node)
                                           (cadddr src-node)))))
    (mk-state0 :data-pos (if (equal data-pos old-data-pos)
                             new-data-pos
                             data-pos)
               :empty-pos old-data-pos
               :nodes nodes
               ;; :nodes (substitute new-dest-node
               ;;                    dest-node
               ;;                    (substitute new-src-node
               ;;                                src-node
               ;;                                nodes
               ;;                                :test 'equal)
               ;;                    :test 'equal)
               )))

;; High-level state space exploration functionality
(defun states-from (state)
  "Return all the reachable states from state."
  (mapcar #'(lambda (pair)
              (let* ((src-node (car pair))
                     (dest-node (cdr pair)))
                (move-data state
                           (cons (car src-node) (cadr src-node))
                           (cons (car dest-node) (cadr dest-node)))))
          (viable-neighbouring-pairs
           (find-node-pos (state-empty-pos state)
                          (state-nodes state))
           
           (state-nodes state))))

(defun is-final-state? (state target-pos)
  "Is state final?"
  (equal (state-data-pos state) target-pos))

(defun adjacent-nodes-used (node nodes)
  "Get the used disk data in each adjacent node to node."
  (mapcar #'cadddr (get-adjacent-nodes node nodes)))

(defun manhattan-distance (pos1 pos2)
  (+ (abs (- (car pos1) (car pos2)))
     (abs (- (cdr pos1) (cdr pos2)))))

(defun score (state final-pos)
  "Evaluate the score of a given state."
  (let* ((data-pos (state-data-pos state))
         (empty-pos (state-empty-pos state)))
    (+ (manhattan-distance data-pos final-pos)
       (manhattan-distance empty-pos data-pos)
       -1)))

(defun sort-states (state-list final-pos)
  "Destructively sort a list of states."
  (sort state-list #'(lambda (s1 s2) (< (score s1 final-pos)
                                        (score s2 final-pos)))))

(defun equal-states? (state1 state2)
  "Are states1 and states2 equal?"
  (let ((data-pos1 (state-data-pos state1))
        (data-pos2 (state-data-pos state2))
        (empty-pos1 (state-empty-pos state1))
        (empty-pos2 (state-empty-pos state2))
        (nodes1 (state-nodes state1))
        (nodes2 (state-nodes state2)))
    (and (equal data-pos1 data-pos2)
         (equal empty-pos1 empty-pos2)
         (equal nodes1 nodes2))))

(defun is-discovered? (state discovered)
  "Was a state previously discovered?"
  (assoc state discovered :test #'equal-states?))

;; State space exploration algorithm(s)
(defun explore-bfs (discovered queue final-pos)
  "(Destructively) explore using BFS starting from queue state."
  (do ((is-final nil))
      ((or is-final (null queue)) (cons is-final discovered))
    (let* ((p (pop queue))
           (state (car p))
           (new-states (sort-states (states-from state) final-pos)))
      (push p discovered)
      (loop for new-state in new-states do
           (when (not (is-discovered? new-state discovered))
             (setq queue (nconc queue (list (cons new-state state)))))
           (when (is-final-state? new-state final-pos)
             (push (cons new-state state) discovered)
             (setq is-final t)
             (return)
             )))))

(defun explore-dfs (discovered stack final-pos &optional (max-depth nil))
  "(Destructively) explore using DFS starting from stack state."
  (do ((is-final nil)
       (stop nil))
      ((or stop (null stack)) (cons is-final discovered))
    (let* ((p (pop stack))
           (state (car p))
           (parent-state (cadr p))
           (depth (caddr p))
           (new-states (sort-states (states-from state) final-pos)))
      (push (cons state parent-state) discovered)
      (if (and max-depth (>= depth max-depth))
          nil
          (loop for new-state in new-states do
               (when (not (is-discovered? new-state discovered))
                 (push (list new-state state (+ 1 depth)) stack))
               (when (is-final-state? new-state final-pos)
                 (push (cons new-state state) discovered)
                 (setq is-final t
                       stop t)
                 (return)
                 ))))))

(defun explore-iddfs (initial-state final-pos)
  (do ((depth 1 (1+ depth))
       (result nil))
      ((not (null result)) result)
    (let ((explore (explore-dfs nil
                                (list (list initial-state 'none 0))
                                final-pos depth)))
      (when (car explore)
        (setq result (cdr explore))))))

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
                       (equal (state-data-pos (car pair))
                              '(0 . 0)))
                       discovered)))
    (when edge
      (car edge))))

(defun find-path (initial final discovered)
  "Find a path from initial to final in discovered."
  (if (equal-states? initial final)
      (list initial)
      (cons final
            (let ((new-final (cdr (is-discovered? final discovered))))
              (find-path initial new-final discovered)))))

(defun solve-grid (initial-state)
  "Solve teh grid computing problem."
  (let* ((discovered (explore-astar initial-state
                                    #'(lambda (state)
                                        (score state '(0 . 0)))
                                    #'states-from
                                    #'equal-states?))
         (final-state (find-final-state discovered)))
    (find-path initial-state final-state discovered)))

;; Tests

;; Part 1
(let ((nodes (with-open-file (in "day22-input")
               (parse-input in))))
  (format t "## Test 1: from day22-input~%")
  (let ((viable (viable-pairs nodes)))
   (format t "Viable pairs are: ~s~%(total ~d)~%"
           viable (length viable))))

;; Part 2
(defvar test-input
  "$ df -h
Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%
")
; Test
(let* ((test-nodes (with-input-from-string (in test-input)
                    (parse-input in)))
       (initial-state (make-state test-nodes)))
  (format t "## Test 0.2: input from test-input...~%")
  (let* ((path (solve-grid initial-state)))
    (format t "Found solution of length ~d.~%" (length path))))

; Actual run
(defvar *sol2*)
(let* ((my-nodes (with-open-file (in "day22-input")
                    (parse-input in)))
       (initial-state (make-state my-nodes)))
  (format t "## Test 2: input from day22-input...~%")
  (let* ((path (solve-grid initial-state)))
    (setq *sol2* path)
    (format t "Found solution of length ~d.~%" (length path))))
