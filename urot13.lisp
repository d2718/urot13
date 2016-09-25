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
(defconstant +parenthesized-small-a+ #x249c)
(defconstant +circled-capital-a+ #x24b6)
(defconstant +circled-small-a+ #x24d0)
(defconstant +parenthesized-capital-a+ #x1f110)
(defconstant +squared-capital-a+ #x1f130)
(defconstant +negative-circled-capital-a+ #x1f150)
(defconstant +negative-squared-capital-a+ #x1f170)
(defconstant +regional-indicator-a+ #x1f1e6)
(defconstant +fullwidth-capital-a+ #xff21)
(defconstant +fullwidth-small-a+ #xff41)

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

(defmacro range-rotate-cond (var lower-limits)
  `(cond ,@(map 'list
               #'(lambda (x)
                   `((and (<= ,x ,var) (> (+ ,x 26) ,var))
                     (+ ,x (mod (+ 13 (- ,var ,x)) 26))))
               lower-limits)
         (t ,var)))

(defun range-rotate (cp)
  "If codepoint CP is in one of the recognized contiguous Latin ranges,
rot13 it; otherwise, pass it through."
  (range-rotate-cond cp (+small-a+ +capital-a+ +small-z+ +capital-z+
                         +parenthesized-small-a+ +circled-capital-a+
                         +circled-small-a+ +parenthesized-capital-a+
                         +squared-capital-a+ +negative-circled-capital-a+
                         +negative-squared-capital-a+ +regional-indicator-a+
                         +fullwidth-capital-a+ +fullwidth-small-a+)))

(defun unicode-rot13 (cp)
  "Decompose precomposed Unicode characters and perform rot13 on the base
ASCII characters. Returns a list of code points for recomposing the rot13'd
output character from combining diacritics. Return a list of a single
code point for non-decomposable characters."
  (let ((cp-list (gethash cp *unicode-map*)))
    (cond (cp-list
           (cons (range-rotate (car cp-list)) (remove-if 'null (cdr cp-list))))
          (t (list (range-rotate cp))))))

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
