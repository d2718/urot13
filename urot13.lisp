;;;
;;; urot13.lisp
;;;
;;; A Unicode-aware rot13 implementation.
;;;
;;; 2016-06-01

(defconstant +small-a+ (char-code #\a))
(defconstant +capital-a+ (char-code #\A))
(defconstant +small-z+ (char-code #\z))
(defconstant +capital-z+ (char-code #\Z))

;; *UNICODE-MAP* will contain a hash that maps a Unicode code point for a
;; precomposed character into a list representing its decomposition into a
;; base ASCII letter and a series of combining diacritics. The CAR of this
;; list is the code point of the base ASCII character, and the CDR is the
;; list of code points of the combining diacritics.
(defparameter *unicode-map* nil)
(let ((udata (with-open-file (f "unicode_data.lisp" :direction :input)
               (read f))))
  (setf *unicode-map* (make-hash-table :size (length udata)))
  (dolist (u udata)
    (setf (gethash (car u) *unicode-map*) (cdr u))))

(defun ascii-rot13 (cp)
  "Perform rot13 on code points representing lower-case or upper-case ASCII
characters. Pass all other code points through."
  (cond ((and (<= +small-a+ cp)
              (>= +small-z+ cp))
         (+ +small-a+
            (mod (+ 13 (- cp +small-a+)) 26)))
        ((and (<= +capital-a+ cp)
              (>= +capital-z+ cp))
         (+ +capital-a+
            (mod (+ 13 (- cp +capital-a+)) 26)))
        (t cp)))

(defun unicode-rot13 (cp)
  "Decompose precomposed Unicode characters and perform rot13 on the base
ASCII characters. Returns a list of code points for recomposing the rot13'd
output character from combining diacritics. Return a list of a single
code point for non-decomposable characters."
  (let ((cp-list (gethash cp *unicode-map*)))
    (cond (cp-list
           (cons (ascii-rot13 (car cp-list)) (remove-if 'null (cdr cp-list))))
          (t (list (ascii-rot13 cp))))))

(defun munch-line (str)
  "Perform rot13 on a line of input."
  (let ((out nil))
    (dolist (c (coerce str 'list))
      (dolist (cp (unicode-rot13 (char-code c)))
        (push cp out)))
    (coerce (map 'list 'code-char (nreverse out)) 'string)))

(defun main ()
  "Print each line of input rot13'd to the standard output."
  (handler-case
      (loop (format t "~a~&" (munch-line (read-line))))
    (end-of-file () nil)))
