;;;
;;; urot13.lisp
;;;
;;; A Unicode-aware rot13 implementation.

(defconstant +small-a+ (char-code #\a))
(defconstant +capital-a+ (char-code #\A))
(defconstant +small-z+ (char-code #\z))
(defconstant +capital-z+ (char-code #\Z))

(defparameter *unicode-map* nil)
(let ((udata (with-open-file (f "unicode_data.lisp" :direction :input)
               (read f))))
  (setf *unicode-map* (make-hash-table :size (length udata)))
  (dolist (u udata)
    (setf (gethash (car u) *unicode-map*) (cdr u))))

(defun ascii-rot13 (cp)
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
  (let ((cp-list (gethash cp *unicode-map*)))
    (cond (cp-list
           (cons (ascii-rot13 (car cp-list)) (remove-if 'null (cdr cp-list))))
          (t (list (ascii-rot13 cp))))))

(defun munch-line (str)
  (let ((out nil))
    (dolist (c (coerce str 'list))
      (dolist (cp (unicode-rot13 (char-code c)))
        (push cp out)))
    (coerce (map 'list 'code-char (nreverse out)) 'string)))

(defun main ()
  (handler-case
      (loop (format t "~a~&" (munch-line (read-line))))
    (end-of-file () nil)))
