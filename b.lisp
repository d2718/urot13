;;; b.lisp
;;;
;;; a build script for urot13

(load "urot13.lisp")
(save-application "urot13"
                  :prepend-kernel t
                  :toplevel-function 'main)
