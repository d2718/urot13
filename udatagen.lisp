;;;
;;; udatagen.lisp
;;;
;;; Generates the Unicode data used by urot13.
;;;
;;; 2016-06-01

;;              Latin-1: #x0080 to #x00ff
;;     Latin Extended-A: #x0100 to #x017f
;;     Latin Extended-B: #x0180 to #x024f
;; Latin Extended Add'l: #x1e00 to #x1eff
;;
;; CCL has usable character literal names up to #x07fa; for Latin Extended
;; Add'l, I'll have to get the names from elsewhere.

(when (null (find-package 'quicklisp))
  (load "~/quicklisp/setup.lisp"))
(when (null (find-package 'cl-ppcre))
  (ql:quickload 'cl-ppcre))

(defconstant +latin-small-letter-a+ (char-code #\a))
(defconstant +latin-small-letter-z+ (char-code #\z))
(defconstant +latin-capital-letter-a+ (char-code #\A))
(defconstant +latin-capital-letter-z+ (char-code #\Z))

(defparameter *combining-diacritics*
  `(("Grave" . #x0300) ("Acute" . #x0301) ("Circumflex" . #x0302)
    ("Tilde" . #x0303) ("Macron" . #x0304) ("Overline" . #x0305)
    ("Breve" . #x0306) ("Dot_Above" . #x0307) ("Diaeresis" . #x0308)
    ("Hook_Above" . #x0309) ("Hook" . #x0309)
    ("Ring_Above" . #x030a) ("Double_Acute" . #x030b)
    ("Caron" . #x030c) ("Vertical_Line_Above" . #x030d)
    ("Double_Vertical_Line_Above" . #x030e) ("Double_Grave" . #x030f)
    ("Candrabindu" . #x0310) ("Inverted_Breve" . #x0311)
    ("Turned_Comma_Above" . #x0312) ("Comma_Above" . #x0313)
    ("Reversed_Comma_Above" . #x0314) ("Comma_Above_Right" . #x0315)
    ("Grave_Accent_Below" . #x0316) ("Acute_Accent_Below" . #x0317)
    ("Left_Tack_Below" . #x0318) ("Right_Tack_Below" . #x0319)
    ("Left_Angle_Above" . #x031a) ("Horn" . #x031b)
    ("Left_Half_Ring_Below" . #x031c) ("Up_Tack_Below" . #x031d)
    ("Down_Tack_Below" . #x031e) ("Plus_Sign_Below" . #x031f)
    ("Minus_Sign_Below" . #x0320)
    ("Palatalized_Hook_Below" . #x0321) ("Palatal_Hook" . #x0321)
    ("Retroflex_Hook_Below" . #x0322) ("Retroflex_Hook" . #x0322)
    ("Hook_Tail" . #x0322)
    ("Dot_Below" . #x0323)
    ("Diaeresis_Below" . #x0324) ("Ring_Below" . #x0325)
    ("Comma_Below" . #x0326) ("Cedilla" . #x0327) ("Ogonek" . #x0328)
    ("Vertical_Line_Below" . #x0329) ("Bridge_Below" . #x032a)
    ("Inverted_Double_Arch_Below" . #x032b) ("Caron_Below" . #x032c)
    ("Circumflex_Below" . #x032d) ("Breve_Below" . #x032e)
    ("Inverted_Breve_Below" . #x032f) ("Tilde_Below" . #x0330)
    ("Macron_Below" . #x0331) ("Line_Below" . #x0331)
    ("Low_Line" . #x0332) ("Double_Low_Line" . #x0333)
    ("Tilde_Overlay" . #x0334) ("Middle_Tilde" . #x0334)
    ("Short_Stroke_Overlay" . #x0335)
    ("Long_Stroke_Overlay" . #x0336) ("Short_Solidus_Overlay" . #x0337)
    ("Long_Solidus_Overlay" . #x0338) ("Right_Half_Ring_Below" . #x0339)
    ("Inverted_Bridge_Below" . #x033a) ("Square_Below" . #x033b)
    ("Seagull_Below" . #x033c) ("X_Above" . #x033d) ("Vertical_Tilde" . #x033e)
    ("Double_Overline" . #x033f) ("Grave_Tone_Mark" . #x0340)
    ("Acute_Tone_Mark" . #x0341) ("Bridge_Above" . #x0346)
    ("Equals_Sign_Below" . #x0347) ("Double_Vertical_Line_Below" . #x0348)
    ("Left_Angle_Below" . #x0349) ("Not_Tilde_Above" . #x034a)
    ("Homothetic_Above" . #x034b) ("Almost_Equal_To_Above" . #x034c)
    ("Left_Right_Arrow_Below" . #x034d) ("Upwards_Arrow_Below" . #x034e)
    ("Right_Arrowhead_Above" . #x0350) ("Left_Half_Ring_Above" . #x0351)
    ("Fermata" . #x0352) ("X_Below" . #x0353) ("Left_Arrowhead_Below" . #x0354)
    ("Right_Arrowhead_Below" . #x0355)
    ("Right_Arrowhead_And_Up_Arrowhead_Below" . #x0356)
    ("Right_Half_Ring_Above" . #x0357) ("Right_Half_Ring" . #x0357)
    ("Dot_Above_Right" . #x0358)
    ("Asterisk_Below" . #x0359) ("Double_Ring_Below" . #x035a)
    ("Zigzag_Above" #x035b) ("Double_Breve_Below" . #x035c)
    ("Double_Breve" . #x035d) ("Double_Macron" . #x035e)
    ("Double_Macron_Below" . #x035f) ("Double_Tilde" . #x0360)
    ("Double_Inverted_Breve" . #x0361)
    ("Double_Rightwards_Arrow_Below" . #x0362)
    ("Small_Letter_A" . #x0363) ("Small_Letter_E" . #x0364)
    ("Small_Letter_I" . #x0365) ("Small_Letter_O" . #x0366)
    ("Small_Letter_U" . #x0367) ("Small_Letter_C" . #x0368)
    ("Small_Letter_D" . #x0369) ("Small_Letter_H" . #x036a)
    ("Small_Letter_M" . #x036b) ("Small_Letter_R" . #x036c)
    ("Small_Letter_T" . #x036d) ("Small_Letter_V" . #x036e)
    ("Small_Letter_X" . #x036f)))

(defparameter *diacritic-name-map*
  (make-hash-table :test 'equal
                   :size (length *combining-diacritics*)))
(dolist (pr *combining-diacritics*)
  (setf (gethash (car pr) *diacritic-name-map*) (cdr pr)))

(with-open-file (f "addl_names.lisp" :direction :input)
  (defconstant +additional-char-names+ (read f)))

(defparameter *additional-char-name-map*
  (make-hash-table :size (length +additional-char-names+)))
(dolist (pr +additional-char-names+)
  (setf (gethash (car pr) *additional-char-name-map*) (cdr pr)))

(defparameter *unichar-re*
  (cl-ppcre:create-scanner "Latin_(Capital|Small)_Letter_([A-Z])_With_(\.+)$"))

(defun codepoint-name (cp)
  (cond ((< cp #x07fb)
         (subseq (format nil "~s" (code-char cp)) 2))
        ((and (< cp #x1f00)
              (>= cp #x1e00))
         (gethash cp *additional-char-name-map*))
        (t nil)))

(defun name-to-ascii (char-case letter)
  (cond ((string= "Capital" char-case)
         (char-code (elt letter 0)))
        ((string="Small" char-case)
         (+ (char-code (elt letter 0))
            (- +latin-small-letter-a+ +latin-capital-letter-a+)))
        (t nil)))

(defun diacritic-name-translate (diacritic-name capital-p)
  (cond ((string= "Stroke" diacritic-name)
         (if capital-p "Long_Stroke_Overlay" "Short_Stroke_Overlay"))
        ((string= "Diagonal_Stroke" diacritic-name)
         (if capital-p "Long_Solidus_Overlay" "Short_Solidus_Overlay"))
        (t diacritic-name)))

(defun unichar-decompose (char-name)
  (cl-ppcre:register-groups-bind
   (char-case char-letter diacritics)
   (*unichar-re* char-name)
   (let ((char-num (name-to-ascii char-case char-letter))
         (diacritic-list (cl-ppcre:split "_And_" diacritics)))
     (values char-num
             (map 'list
                  #'(lambda (x)
                      (gethash (diacritic-name-translate
                                x (string= "Capital" char-case)) 
                               *diacritic-name-map*))
                  diacritic-list)))))

(defun main ()
  (let ((data nil))
    (loop for n from #x0080 to #x024f do
         (multiple-value-bind (base-code combined-codes)
             (unichar-decompose (codepoint-name n))
           (when (and base-code combined-codes)
             (push (cons n (cons base-code combined-codes))
                   data))))
    (loop for n from #x1e00 to #x1eff do
         (multiple-value-bind (base-code combined-codes)
             (unichar-decompose (codepoint-name n))
           (when (and base-code combined-codes)
             (push (cons n (cons base-code combined-codes))
                   data))))
    (with-open-file (f "unicode_data.lisp" :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format f "~s" data))
    (format t "Wrote ~a data points.~&" (length data))))

(defun list-nulls ()
  (let ((unidata (with-open-file (f "unicode_data.lisp" :direction :input)
                   (read f)))
        (nullz nil))
    (dolist (x unidata)
      (when (position nil x)
        (push x nullz)))
    nullz))

(defun list-names (lst)
  (dolist (x lst)
    (format t "~x (~a) ~a~&" (first x) (first x) (codepoint-name (first x)))))

(defun hex (x) (format nil "~x" x))
