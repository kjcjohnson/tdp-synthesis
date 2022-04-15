;;;
;;; Perfect Propagation 2 - single-child LIA
;;;
(in-package #:com.kjcjohnson.tdp.test)

(defun pp-2-g ()
  "Generates the pp2 grammar."
  `(
    ("+1" Term "+1" Term)
    ("x2" Term "x2" Term)
    ("Input" Term "<input>")
    ("Constant" Term "*")))

(defparameter *pp-2-g*
  (g:make-rtg :productions (pp-2-g)))

(semgus:defsemantics *pp-2-sem*
  :operational
  ((Tuple (is)
          ("+1" (sem1) ((1+ (funcall sem1 is))))
          ("x2" (sem1) ((* 2 (funcall sem1 is))))
          ("Input" () (is))
          ("Constant" (val) ((funcall val is))))))

(defparameter *pp-2-witness*
  #'(lambda (prod-name child-index output)
      (if (= 0 child-index)
          
          (cond
            ((string= prod-name "+1")
             (if (not (zerop output))
                 (1- output)))
            ((string= prod-name "x2")
             (if (and (evenp output) (not (zerop output)))
                 (/ output 2)
                 nil)))
          nil)))
