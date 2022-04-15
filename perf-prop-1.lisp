;;;
;;; Perfect Propagation 1 - tuples
;;;
(in-package #:com.kjcjohnson.tdp.test)

(defun pp-1-g ()
  "Generates the dr-name grammar."
  `(
    ("Single" Tuple "[1]" Tuple)
    ("Double" Tuple "[2]" Tuple Tuple)
    ("Triple" Tuple "[3]" Tuple Tuple Tuple)
    ("Input" Tuple "<input>")
    ("Constant" Tuple "*")))

(defparameter *pp-1-g*
  (g:make-rtg :productions (pp-1-g)))

(semgus:defsemantics *pp-1-sem*
  :operational
  ((Tuple (is)
          ("Single" (sem1) ((list (funcall sem1 is))))
          ("Double" (sem1 sem2) ((list (funcall sem1 is)
                                       (funcall sem2 is))))
          ("Triple" (sem1 sem2 sem3) ((list (funcall sem1 is)
                                            (funcall sem2 is)
                                            (funcall sem3 is))))
          ("Input" () (is))
          ("Constant" (val) ((funcall val is))))))

;;;
;;; Witness functions for available productions
;;;
(defparameter *pp-1-witness*
  #'(lambda (prod-name child-index context-info)
    (cond
      ((string= prod-name "Single")
       (if (and (consp context-info)
                (= 1 (length context-info)))
           (first context-info)
           nil))
      ((string= prod-name "Double")
       (if (and (consp context-info)
                (= 2 (length context-info)))
           (nth child-index context-info)
           nil))
      ((string= prod-name "Triple")
       (if (and (consp context-info)
                (= 3 (length context-info)))
           (nth child-index context-info)
           nil)))))
