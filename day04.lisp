;; Advent of Code 2016, Day 04
(require "cl-ppcre")

;; Problem description:
;
; You will receive lines of the form:
;
; - multiple alphabetic letters and dashes
; - a dash, then a numeric ID
; - a sequence of alphabetic letters enclosed in square brackets
;
; The sequence in the square brackets form a checksum of the alphabetic
; letters from the beginning of the sequence. We need to check whether
; the checksum is passed.
;
; The checksum is obtained by counting the letter frequencies of each
; letter in the alphabet. The most frequent letter comes first. If two
; letters have the same frequency, they are taken in alphabetic
; order. The checksum formed by selecting the five most frequent letters
; obtained according to these criteria.
;
; The problem asks to get the sum of the numeric IDs of the lines whose
; checksums match the reference.
;;

;; Part 1:
;
; The solution is straightforward: parse the line, compute the checksum
; and if that matches, add the ID towards the sum.
;
; Parsing the line can be done using a regular expression (see below).
;
; Computing the checksum requires getting the alphabetic letters and
; counting the number of appearances, thus essentially getting a
; multiset of the letters. We sort the letters according to the two
; criteria (number of appearances and alphabetic appearances), then we
; take the first five letters.
;;

(defun take (n L)
  "Return n elements from L."
  (if (or (= n 0) (null L))
      nil
      (cons (car L) (take (- n 1) (cdr L)))))

(defun parse-line (line)
  "Parse line according to problem description."
  (let ((scan (multiple-value-bind (_ strings)
                  (cl-ppcre:scan-to-strings
                   "([a-z-]+)-([0-9]+)\\[([a-z][a-z][a-z][a-z][a-z])\\]"
                   line)
                (declare (ignore _))
                strings)))
    (when scan
     (list (remove #\- (elt scan 0))
           (parse-integer (elt scan 1))
           (elt scan 2)))))

(defun make-multiset (str)
  "Makes a multiset (letter-frequency hash table) out of a string."
  (let ((S (make-hash-table)))
    (loop for c across str
             do (if (gethash c S)
                    (incf (gethash c S))
                    (setf (gethash c S) 1)))
    S))

(defun sorted-multiset-pairs (S)
  "Return a list of the multiset elements sorted according to the
problem description."
  (let ((L (loop for key being the hash-keys of S
              using (hash-value value)
              collect (cons key value))))
    (sort L #'(lambda (p1 p2)
                (if (= (cdr p1) (cdr p2))
                    (< (char-code (car p1)) (char-code (car p2)))
                    (> (cdr p1) (cdr p2)))))))

(defun compute-checksum (str)
  "Compute checksum according to problem description."
  (let* ((S (make-multiset str))
         (first-five (take 5 (sorted-multiset-pairs S))))
    (coerce (mapcar #'car first-five) 'string)))

(defun checksum-match-id (triple)
  "If the checksum of a letter-ID-checksum triple matches, return the
ID. Otherwise return nil."
  (let ((checksum (compute-checksum (car triple))))
    (when (string= checksum (caddr triple))
      (cadr triple))))

(defun go-bibi (input-stream)
  "Solve the problem (Part 1)."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (sum 0))
      ((null line) sum)
    (let ((match (checksum-match-id (parse-line line))))
      (when match (incf sum match)))))

;; Part 2
;
; The strings to the left are encrypted using rot-ID encryption: each
; letter is rotated through the alphabet using the ID as a key. For
; example, for example "a" rotated with 2 becomes "c".
;
; qzmt-zixmtkozy-ivhz-343[.....] is "very encrypted name"
;;

(defvar alpha-mod-num (- (char-code #\z) (char-code #\a) -1))
(defun rotate-letter (c n)
  "Rotate c by n positions."
  (let ((offset-to-a (- (char-code c) (char-code #\a))))
    (code-char (+ (char-code #\a)
                  (mod (+ offset-to-a n) alpha-mod-num)))))

(defun rotate-string (str n)
  "Rotate each character c in str by n positions."
  (let ((L (coerce str 'list)))
    (coerce (mapcar #'(lambda (c) (rotate-letter c n)) L) 'string)))

(defun go-bibi2 (input-stream)
  "Solve Part 2 of the problem."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (results nil))
      ((null line) results)
    (let* ((triple (parse-line line))
           (cipher (car triple))
           (match (checksum-match-id triple))
           (cleartext (when match (rotate-string cipher match))))
      (when (and match (search "north" cleartext))
        (push (cons cleartext triple) results)))))

;; Tests
(defvar test0-str
  (format nil "~a~%~a~%~a~%~a~%"
          "aaaaa-bbb-z-y-x-123[abxyz]"
          "a-b-c-d-e-f-g-h-987[abcde]"
          "not-a-real-room-404[oarel]"
          "totally-real-room-200[decoy]"))
(with-input-from-string (in test0-str)
  (format t "## Test 0.1: test0-str~%ID Sum is: ~d"
          (go-bibi in)))

(format t "## Test 0.2: qzmtzixmtkozyivhz-343~%Decrypted text is: ~a~%"
        (rotate-string "qzmtzixmtkozyivhz" 343))

(with-open-file (in "day04-input")
  (format t "## Test 1: from test04-input~%ID Sum is: ~d~%"
          (go-bibi in)))

(with-open-file (in "day04-input")
  (format t "## Test 2: from test04-input~%Got: ~s"
          (go-bibi2 in)))
