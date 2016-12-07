;; Advent of Code 2016, Day 06
(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :cl-ppcre))

;; Problem description:
;
; We are given as input a list of IPv7 addresses. An IPv7 address has
; the following format: a bunch of sequences of alphabetic characters,
; some of which are enclosed in square brackets. We call the sequences
; enclosed in square brackets hypernets.
;
; For example "abcd[xyzw]efgh" is a valid IPv7 address, where the first
; (regular) sequence is "abcd", "xyzw" is a hypernet sequence and the
; second sequence is "efgh".
;
; We want to figure out which IPv7 addresses suport Transport-Layer
; Snooping (TLS). An IPv7 address supports TLS if it contains an
; Autonomous Bridge Bypass Annotation (ABBA) within regular
; (non-hypernet) sequences, *and* if it doesn't contain an ABBA in any
; hypernet sequences.
;
; An ABBA is a four-character palindrome sequence consisting of
; different characters, i.e. two different characters followed by their
; reverse. For example "aaaa" and "abcd" are not ABBAs, but "cddc" and
; "abba" are.
;
; Further examples:
;
; - abba[mnop]qrst supports TLS ("abba" in group1)
;
; - abcd[bddb]xyyx does not support TLS (although we have "xyyx" in
;   group2, we also have "bddb" in the hypernet)
;
; - aaaa[qwer]tyui does not support TLS ("aaaa" is not a ABBA, although
;   it is a palindrome)
;
; - ioxxoj[asdfgh]zxcvbn supports TLS ("oxxo" is an ABBA)
;;

(defun parse-ipv7 (line)
  "Parse an IPv7 address according to the problem description.

An IPv7 structure is a list whose elements are lists containing an
alphabetic string, plus an optional property :hypernet which is set to t
when the sequence is a hypernet."
  (let ((tokens (cl-ppcre:split "(\\[)|(\\])" line
                                :with-registers-p t
                                :omit-unmatched-p t)))
    (when tokens
      ; Go through the token stream and parse potential hypernet
      ; sequences.
      (do ((tokstream tokens (cdr tokstream))
           (ipv7 nil))
          ((null tokstream) (reverse ipv7))
        (if (string= "[" (car tokstream))
            (progn (pop tokstream)
                   (push (list :seq (car tokstream) :hypernet t) ipv7)
                   (pop tokstream))
            (push (list :seq (car tokstream)) ipv7))))))

(defmacro n-char-subseqs (n str)
  "Return a list of all the four-character subsequences in str."
  `(loop for i from 0 to (- (length ,str) ,n)
      collect (subseq ,str i (+ i ,n))))

(defun contains-abba (str)
  "If str contains an ABBA, return it."
  (find-if #'(lambda (four-char-str)
               (and
                (not (eq (elt four-char-str 0)
                         (elt four-char-str 1)))
                (string= (subseq four-char-str 0 2)
                         (reverse (subseq four-char-str 2 4)))))
           (n-char-subseqs 4 str)))

(defmacro is-abba-hypernet (seq)
  `(and (getf ,seq :hypernet)
        (contains-abba (getf ,seq :seq))))

(defmacro is-abba-regular (seq)
  `(and (not (getf ,seq :hypernet))
        (contains-abba (getf ,seq :seq))))

(defun supports-tls (ipv7)
  "Checks whether a given IPv7 address supports TLS."
  (let ((abba nil))
    (loop for seq in ipv7
       do (cond
            ((is-abba-hypernet seq) (setq abba nil) (return nil))
            ((and (not abba) (is-abba-regular seq))
             (setq abba (getf seq :seq)))))
    abba))

(defun go-bibi (input-stream)
  "Solve the problem (Part 1)."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (counter 0))
      ((null line) counter)
    (when (supports-tls (parse-ipv7 line))
      ;(format t "~a supports tls.~%" line)
      (incf counter))))

;; Part 2
;
; We find out about now that regular sequences are called supernet
; sequences. Great!
;
; We want to find out the IPv7 addresses that support Super-Secret
; Listening (SSL). An IPv7 address supports SSL if it has an
; Area-Broadcast Accessor (ABA) in any supernet sequence and a
; *corresponding* Byte-Allocation Block (BAB) in any hypernet
; sequence.
;
; An ABA is a three-letter sequence whose first and last letter are the
; same. A BAB is a three-letter sequence is the same, but with the
; reversed positions. For example, where "aba" is an ABA, "bab" is a
; BAB; "xyx"-"yxy", "cdc"-"dcd", "yty"-"tyt" are all valid ABA-BAB
; pairs.
;
; Further examples:
;
; - aba[bab]xyz supports SSL
;
; - xyx[xyx]xyx does not support SSL (BAB not found)
;
; - aaa[kek]eke supports SSL ("eke"-"kek" is a valid ABA-BAB pair)
;
; - zazbz[bzb]cdb supports SSL ("zbz"-"bzb" is a valid ABA-BAB pair)
;;

(defun contains-aba-bab (str)
  "Return all instances of ABA/BAB in str."
  (remove-if-not #'(lambda (three-char-str)
                     (and
                      (not (eq (elt three-char-str 0)
                               (elt three-char-str 1)))
                      (eq (elt three-char-str 0)
                          (elt three-char-str 2))))
           (n-char-subseqs 3 str)))

(defun aba->bab (str)
  "Converts an ABA to a BAB."
  (coerce (list (elt str 1)
                (elt str 0)
                (elt str 1))
          'string))

(defun intersect-str (L1 L2)
  "Intersect two sets of strings, represented as lists."
  (let ((result nil))
    (loop for e1 in L1 do
         (loop for e2 in L2 do
              (when (string= e1 e2)
                (push e1 result))))
    result))

(defun supports-ssl (ipv7)
  "Return a list of all the ABA pairs indicating that SSL is supported."
  (let ((abas nil)
        (babs nil))
    (loop for seq in ipv7 do
         (if (getf seq :hypernet)
             (dolist (bab (contains-aba-bab (getf seq :seq)))
               (push bab babs))
             (dolist (aba (contains-aba-bab (getf seq :seq)))
               (push aba abas))))
    (intersect-str abas (mapcar #'aba->bab babs))))

(defun go-bibi2 (input-stream)
  "Solve the problem (Part 2)."
  (do ((line (read-line input-stream nil) (read-line input-stream nil))
       (counter 0))
      ((null line) counter)
    (when (supports-ssl (parse-ipv7 line))
      ;(format t "~a supports ssl.~%" line)
      (incf counter))))

;; Tests
(defvar test0.1-input
  (format nil "~a~%~a~%~a~%~a~%"
          "abba[mnop]qrst" "abcd[bddb]xyyx" "aaaa[qwer]tyui"
          "ioxxoj[asdfgh]zxcvbn"))
(defvar test0.2-input
  (format nil "~a~%~a~%~a~%~a~%"
          "aba[bab]xyz" "xyx[xyx]xyx"
          "aaa[kek]eke" "zazbz[bzb]cdb"))

(with-input-from-string (in test0.1-input)
  (format t "## Test 0.1: ~d addrs (test0.1-input) support TLS.~%"
          (go-bibi in)))
(with-input-from-string (in test0.2-input)
  (format t "## Test 0.2: ~d addrs (test0.2-input) support SSL.~%"
          (go-bibi2 in)))

(with-open-file (in "day07-input")
  (format t "## Test 1: ~d addrs (day07-input) support TLS.~%"
          (go-bibi in)))
(with-open-file (in "day07-input")
  (format t "## Test 2: ~d addrs (day07-input) support SSL.~%"
          (go-bibi2 in)))
