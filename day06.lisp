;; Advent of Code 2016, Day 06

;; Problem description:
;
; We have a simple, straightforward error correction scheme to
; implement. The input consists of a message M that's sent repeatedly,
; each instance of M on a separate line of the input file.
;
; We can reconstitute each letter of M by getting the most frequent
; symbol that appears in all the instances. The end result should
; hopefully be the initial M.
;;

; We solve this using an array of hash tables, each hash table
; representing a multiset holding the occurences of each letter in a
; given position along the instances of received messages.

(defmacro arr-end (arr)
  "Compute the end index of an array (to be passed to loop)."
  `(- (array-dimension ,arr 0) 1))

(defun initialize-mmset (line)
  "Initialize a multi-multiset."
  (let ((arr (make-array (length line))))
    (loop for i from 0 to (arr-end arr)
         do (setf (aref arr i) (make-hash-table)))
    arr))

(defun update-mmset (mmset line)
  "Update a multi-multiset with a given value."
  (loop for i from 0 to (arr-end mmset)
       do (let ((c (elt line i)))
            (if (null (gethash c (aref mmset i)))
                (setf (gethash c (aref mmset i)) 1)
                (incf (gethash c (aref mmset i))))))
  mmset)

(defun occurence-list (mset)
  "Get an occurence list from a hash table."
  (loop for k being the hash-keys of mset
       using (hash-value v)
       collect (cons k v)))

(defun get-frequency (mset f)
  "Reduce the mset over a frequency function f."
  (reduce f (occurence-list mset)))

(defun get-most-frequent (mset)
  "Get the most frequent character in a multiset."
  (get-frequency mset
                 #'(lambda (acc p)
                     (if (> (cdr p) (cdr acc)) p acc))))

(defun mmset->string (mmset f)
  "Decode a mmset to a human-readable string, retrieving the initial
message according to a character retrieval function f."
  (map 'string
       #'(lambda (mset) (car (funcall f mset)))
       mmset))

(defun go-bibi (input-stream)
  "Solve the problem (Part 1)."
  ; Read the first line, assume it exists.
  (let* ((line (read-line input-stream nil))
         (mmset (initialize-mmset line)))
    (update-mmset mmset line)
    (do ((line (read-line input-stream nil) (read-line input-stream nil)))
        ((null line) (mmset->string mmset #'get-most-frequent))
      (update-mmset mmset line))))

;; Part 2
;
; Same as Part 1, only we now choose the *least* frequent character over
; all the instances of M.
;;

(defun get-least-frequent (mset)
  "Get the most frequent character in a multiset."
  (get-frequency mset
                 #'(lambda (acc p)
                     (if (< (cdr p) (cdr acc)) p acc))))

(defun go-bibi2 (input-stream)
  "Solve the problem (Part 2)."
  ; Read the first line, assume it exists.
  (let* ((line (read-line input-stream nil))
         (mmset (initialize-mmset line)))
    (update-mmset mmset line)
    (do ((line (read-line input-stream nil) (read-line input-stream nil)))
        ((null line) (mmset->string mmset #'get-least-frequent))
      (update-mmset mmset line))))

;; Tests
(defvar test0-input
  (apply #'concatenate 'string
         (mapcar #'(lambda (line) (format nil "~a~%" line))
                 '("eedadn" "drvtee" "eandsr" "raavrd"
                   "atevrs" "tsrnev" "sdttsa" "rasrtv"
                   "nssdts" "ntnada" "svetve" "tesnvt"
                   "vntsnd" "vrdear" "dvrsen" "enarar"))))

(with-input-from-string (in test0-input)
  (format t "## Test 0.1: input from test0-input~%Message is: ~a~%"
          (go-bibi in)))
(with-input-from-string (in test0-input)
  (format t "## Test 0.2: input from test0-input~%Message is: ~a~%"
          (go-bibi2 in)))

(with-open-file (in "day06-input")
  (format t "## Test 1: input from day06-input~%Message is: ~a~%"
          (go-bibi in)))
(with-open-file (in "day06-input")
  (format t "## Test 2: input from day06-input~%Message is: ~a~%"
          (go-bibi2 in)))
