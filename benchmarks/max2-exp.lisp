;;;
;;; max2-exp
;;;
(in-package #:com.kjcjohnson.tdp.test)

(defparameter *max2-exp-g*
  (g:make-rtg :productions '(
                             ("x" Start x)
                             ("y" Start y)
                             ("0" Start "0")
                             ("1" Start "1")
                             ("+" Start + Start Start)
                             ("-" Start - Start Start)
                             ("ite" Start ite B Start Start)
                             ("true" B true)
                             ("false" B false)
                             ("not" B not B)
                             ("and" B and B B)
                             ;("<angelic>" B angel)
                             ("<" B < Start Start))))


(semgus:defsemantics *max2-exp-sem*
  :operational
  ((Start (is)
          ("x" ()
               ((cdr (assoc :x is))))
          ("y" ()
               ((cdr (assoc :y is))))
          ("0" ()
               ((declare (ignore is)) 0))
          ("1" ()
               ((declare (ignore is)) 1))
          ("+" (sem1 sem2)
               ((+ (funcall sem1 is) (funcall sem2 is))))
          ("-" (sem1 sem2)
               ((- (funcall sem1 is) (funcall sem2 is))))
          ("ite" (semb semt seme)
                 ((if (funcall semb is)
                      (funcall semt is)
                      (funcall seme is)))))
   (B (is)
      ("true" ()
              ((declare (ignore is)) t))
      ("false" ()
               ((declare (ignore is)) (values nil t)))
      ("not" (sem1)
             ((values (not (funcall sem1 is)) t)))
      ("and" (sem1 sem2)
             ((values (and (funcall sem1 is) (funcall sem2 is)) t)))
      ("<" (sem1 sem2)
           ((values (< (funcall sem1 is) (funcall sem2 is)) t))))))
