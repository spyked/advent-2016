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

;; XXX: useless.
(defun get-adjacent-nodes (node nodes)
  "Get adjacent nodes for a given node."
  (remove-if #'null
             (mapcar #'(lambda (node offset)
                         (find-node (list (+ (car node) (car offset))
                                          (+ (cadr node) (cdr offset))
                                          0 0) nodes))
                     (list node node node node)
                     (list '(0 . 1) '(1 . 0) '(0 . -1) '(-1 . 0)))))

;; XXX: useless.
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

;; Tests
(let ((nodes (with-open-file (in "day22-input")
               (parse-input in))))
  (format t "## Test 1: from day22-input~%")
  (let ((viable (viable-pairs nodes)))
   (format t "Viable pairs are: ~s~%(total ~d)~%"
           viable (length viable))))
