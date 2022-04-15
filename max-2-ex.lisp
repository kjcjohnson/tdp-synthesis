(in-package #:com.kjcjohnson.tdp.test)

(defun max2-g ()
  `(
    ("0" E "0")
    ("1" E "1")
    ("x" E "x")
    ("y" E "y")
    ("+" E "+" E E)
    ("ite" E "ite" B E E)
    ("T" B)
    ("F" B)
    ("<" B "<" E E)))

(defparameter *max2-g* (g:make-rtg :productions (max2-g)))

(semgus:defsemantics *max2-sem*
  :operational
  ((E (is)
      ("0" () ((declare (ignore is)) 0))
      ("1" () ((declare (ignore is)) 1))
      ("x" () ((cdr (assoc :x is))))
      ("y" () ((cdr (assoc :y is))))
      ("+" (sem1 sem2)
           ((+ (funcall sem1 is) (funcall sem2 is))))
      ("ite" (sem1 sem2 sem3)
             ((if (funcall sem1 is) (funcall sem2 is) (funcall sem3 is)))))
   (B (is)
      ("T" () ((declare (ignore is)) (t)))
      ("F" () ((declare (ignore is)) (nil)))
      ("<" (sem1 sem2) ((< (funcall sem1 is) (funcall sem2 is)))))))
