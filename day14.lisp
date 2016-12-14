;; Advent of Code 2016, Day 14
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :sb-md5)) ; my choice of md5 digest implementation

;; Problem description:
;
; MD5 stuff again.
;
; We get as input a random sequence of characters (e.g. abc) and a
; number, incrementally starting from 0. We get abc0, abc1, etc. and we
; MD5 them, looking for what we call a *key* (for a one-time pad, but
; that's irrelevant for our puzzle).
;
; An MD5 hash is a key if and only if a character appears three times
; consecutively in the hash, e.g. ...ddd..., *and* the same character
; appears five times consecutively in one of the 1000 next hashes.
;
; For example, if the input is abc:
;
; - abc18 produces a triple ...cc38887a5..., but none of the next 1000
;   hashes (19 through 1018) contains 88888.
;
; - abc39 produces eee, and abc816 produces eeeeee.
;
; - abc92 contains 999, and 200 contains 99999.
;
; - abc22728 generates the 64th key.
;
; The problem asks us to determine which index produces the 64th key.
;;

; From day05, a bit modified
(defun get-md5-hash (str)
  "Obtain the MD5 hash of str as a string."
  ; md5sum-string returns an array of bytes, which we turn into a list
  ; and display using hex; then we concatenate the result.
  (apply #'concatenate 'string
         (map 'list #'(lambda (byte)
                        (format nil "~(~2,'0x~)" byte))
                 (sb-md5:md5sum-string str))))

; From day05
(defun make-input (id iterator)
  "Craft an input to find password."
  (concatenate 'string id (format nil "~d" iterator)))

(defun make-md5-stream (id &optional (iterator 0))
  "Build a stream of MD5 hashes of secret string id, concatenatively
iterated by iterator."
  (cons (list iterator (get-md5-hash (make-input id iterator)))
        #'(lambda () (make-md5-stream id (1+ iterator)))))

(defun eval-md5-stream (stream n &optional (until nil))
  "Take the first n elements of an MD5 stream."
  (cond
    ((= n 0) stream)
    ((and until (funcall until (car stream))) stream)
    (t (let ((head (car stream))
             (rest (if (functionp (cdr stream))
                       (funcall (cdr stream))
                       (cdr stream))))
         (setf (cdr stream) rest)
         (cons head
               (eval-md5-stream (cdr stream) (- n 1) until))))))

(defun take (L n &optional (until nil))
  (if (or (null L) (= n 0) (and until (funcall until (car L))))
      nil
      (cons (car L) (take (cdr L) (- n 1) until))))

(defun drop (L n &optional (until nil))
  (if (or (null L) (= n 0) (and until (funcall until (car L))))
      L
      (drop (cdr L) (- n 1) until)))

(defun take-md5-stream (stream n &optional (until nil))
  (take (eval-md5-stream stream n until) n until))

(defun drop-md5-stream (stream n &optional (until nil))
  (drop (eval-md5-stream stream n until) n until))

(defmacro repeat-str (c n)
  "Repeat c n times, generating a string."
  (let ((acc (gensym))
        (k (gensym)))
    `(do ((,acc nil)
          (,k 0 (1+ ,k)))
         ((= ,k ,n) (coerce ,acc 'string))
       (push ,c ,acc))))

(defun has-repeating-char (str n)
  "If a char repeats n times in str, return it."
  (let ((last-index (- (length str) n)))
    (loop for i from 0 to last-index do
         (when (string= (subseq str i (+ i n))
                        (repeat-str (elt str i) n))
           (return (elt str i))))))

(defun find-key (md5-stream)
  "Find a key in an md5-stream, as described in problem description."
  (let* ((three-chars (drop-md5-stream md5-stream -1
                                       #'(lambda (l)
                                           (has-repeating-char (cadr l) 3))))
         (repeating-char (has-repeating-char (cadar three-chars) 3)))
    (assert (= (length (cadar three-chars)) 32))
    ; Generate the rest of the stream
    (eval-md5-stream three-chars 1)
    (let* ((rpt5 (repeat-str repeating-char 5))
           (five-chars (drop-md5-stream (cdr three-chars) 1000
                                        #'(lambda (l)
                                            (search rpt5 (cadr l))))))
      ;(print (cons (car three-chars) (car five-chars)))
      (if (search rpt5 (cadar five-chars))
          (progn
            (format t "Found key: ~s~%" (car three-chars))
            (values (car three-chars) (cdr three-chars)))
          (find-key (cdr three-chars))))))

(defun find-keys (md5-stream n)
  "Find the first n keys in an md5-stream"
  (if (= 0 n)
      nil
      (multiple-value-bind (key rest)
          (find-key md5-stream)
        (cons key (find-keys rest (- n 1))))))

;; Part 2
;
; The algorithm is the same, but the input stream is generated
; differently. Each input + index is hashed not once, but a total 2017
; times.
;
; For example:
;
; - md5(abc0) = 577571be4de9dcce85a041ba0410f29f
; - md5(577571be4de9dcce85a041ba0410f29f) = eec80a0c92dc8a0777c619d9bb51e910
; - md5(eec80a0c92dc8a0777c619d9bb51e910) = 16062ce768787384c81fe17a7a60c7e3
; ...
; - md5(...) = a107ff634856bb300138cac6568c0f24
;
; That is how a hash in the stream is generated; the 3-5 checks remain
; the same.
;;
(defun get-md5-hash-times (str n)
  "Hash str sequentially n times."
  (do* ((k 0 (1+ k))
        (s str)
        (h (get-md5-hash s) (get-md5-hash s)))
       ((= k n) s)
    (setq s h)))

(defun make-md5-stream2 (id &optional (iterator 0))
  "Starting from secret string id and the iterator, build a stream of
strings hashed 2017 times."
  (cons (list iterator (get-md5-hash-times (make-input id iterator) 2017))
        #'(lambda () (make-md5-stream2 id (1+ iterator)))))

;; Tests
(let ((test1.0-input "abc"))
  (format t "## Test 1.0: ~a~%" test1.0-input)
  (let* ((stream (make-md5-stream test1.0-input))
         (keys (find-keys stream 64)))
    (format t "64th key is: ~s~%" (elt keys 63)))
  (format t "## Test 2.0: ~a~%" test1.0-input)
  (let* ((stream (make-md5-stream2 test1.0-input))
         (keys (find-keys stream 64)))
    (format t "64th key is: ~s~%" (elt keys 63))))

(let ((test-input "qzyelonm"))
  (format t "## Test 1.1: ~a~%" test-input)
  (let* ((stream (make-md5-stream test-input))
         (keys (find-keys stream 64)))
    (format t "~s~%" keys)
    (format t "64th key is: ~s~%" (elt keys 63)))
  (format t "## Test 2.1: ~a~%" test-input)
  (let* ((stream (make-md5-stream2 test-input))
         (keys (find-keys stream 64)))
    (format t "64th key is: ~s~%" (elt keys 63))))

