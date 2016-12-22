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

;; Stolen from day 11.
;; XXX might need to use streams if outputs are large enough.
;;
;; XXX This is not useful! We need to group *adjacent* pairs in our
;; network topology.
(defun combs-of-at-most (S k &optional (acc nil))
  "Combinations of elements in S in groups of at most k."
  (if (or (null S) (= 0 k))
      (list acc)
      (append (combs-of-at-most (remove (car S) S) (- k 1) (cons (car S) acc))
              (combs-of-at-most (cdr S) k acc))))

(defun combs-of (S k)
  "Combinations of elements in S in groups of exactly k."
  (remove-if-not #'(lambda (L) (= (length L) k))
                 (combs-of-at-most S k)))

;; Tests
(let ((nodes (with-open-file (in "day22-input")
               (parse-input in))))
  ;(print nodes)
  ;(print (combs-of nodes 2))
  )
