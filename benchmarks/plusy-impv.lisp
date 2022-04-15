;;;
;;; plusy-impv
;;;
(in-package #:com.kjcjohnson.frangel.test)

(defun plusy-impv-g (&key angelic?)
  "Generates the plusy-impv grammar."
  `(
   ("x=" Start x= E)
   ("y=" Start y= E)
   ("c=" Start c= E)
   ("y--" Start y--)
   ("site" Start site #|,(if angelic? 'Angel 'B)|# B Start Start)
   (";" Start ";" Start Start)
   ("eps" Start "eps")
   ("while" Start while ,(if angelic? 'Angel 'B) Start)
   ("x" E x)
   ("y" E y)
   ("c" E c)
   ("0" E "0")
   ("1" E "1")
   ("+" E + E E)
   ("-" E - E E)
   ("true" B true)
   ("false" B false)
   ("not" B not B)
   ("and" B and B B)
   ("<" B < E E)
   ,@(when angelic? '(("<angelic>" Angel angel)))))

(defparameter *plusy-impv-g*
  (g:make-rtg :productions (plusy-impv-g :angelic? nil)))

(defparameter *plusy-impv-g-a*
  (g:make-rtg :productions (plusy-impv-g :angelic? t)))


(defparameter *composition-plusy-op-1* (grab-op *plusy-impv-g* ";"))
(defparameter *composition-plusy-op-2* (grab-op *plusy-impv-g-a* ";"))
(defmethod ast:print-program-operator ((op (eql *composition-plusy-op-1*)) children stream)
  (format stream "~{~A~^; ~}" children))
(defmethod ast:print-program-operator ((op (eql *composition-plusy-op-2*)) children stream)
  (format stream "~{~A~^; ~}" children))


(defparameter *site-plusy-op-1* (grab-op *plusy-impv-g* "site"))
(defparameter *site-plusy-op-2* (grab-op *plusy-impv-g-a* "site"))
(defmethod ast:print-program-operator ((op (eql *site-plusy-op-1*)) children stream)
  (format stream "if (~A) then {~A} else {~A}" (first children) (second children) (third children)))
(defmethod ast:print-program-operator ((op (eql *site-plusy-op-2*)) children stream)
  (format stream "if (~A) then {~A} else {~A}" (first children) (second children) (third children)))


(semgus:defsemantics *plusy-impv-sem*
  :operational
  ((Start (is)
          ("x=" (sem1) ((alist-update :x (funcall sem1 is) is)))
          ("y=" (sem1) ((alist-update :y (funcall sem1 is) is)))
          ("c=" (sem1) ((alist-update :c (funcall sem1 is) is)))
          ("y--" () ((alist-update :y (1- (cdr (assoc :y is))) is)))
          (";" (sem1 sem2) ((funcall sem2 (funcall sem1 is))))
          ("eps" () (is))
          ("while" (semb sem1)
                   ((let ((res
                            (loop with next-state = is
                                  for limit from 0 to 1000
                                  when (= limit 20) do (ast::abort-program-execution)
                                    while (funcall semb next-state)
                                  doing (setf next-state (funcall sem1 next-state))
                                  finally (return next-state))))
                      ;(format *trace-output* "; LRES: ~a~%" res)
                      res)))
          ("site" (semb semt seme)
                 ((if (funcall semb is)
                      (funcall semt is)
                      (funcall seme is)))))
   (E (is)
          ("x" ()
               ((cdr (assoc :x is))))
          ("y" ()
               ((cdr (assoc :y is))))
          ("c" ()
               ((cdr (assoc :c is))))
          ("0" ()
               ((declare (ignore is)) 0))
          ("1" ()
               ((declare (ignore is)) 1))
          ("+" (sem1 sem2)
               ((+ (funcall sem1 is) (funcall sem2 is))))
          ("-" (sem1 sem2)
               ((- (funcall sem1 is) (funcall sem2 is)))))
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
           ((values (< (funcall sem1 is) (funcall sem2 is)) t))))

   (Angel (is)
          ("<angelic>" ()
                       ((declare (ignore is)) (values (f::angel) t)))))
  :sorts ((Int smt:*int-sort*) (Bool smt:*bool-sort*))
  :variables ((x Int) (y Int) (c Int)
              (rx Int) (ry Int) (rc Int)
              (r Int) (r1 Int) (r2 Int) (r3 Int)
              (b Bool) (b1 Bool) (b2 Bool))
  :relational
  ((Start (s.sem Int Int Int Int Int Int Int)
          ("x=" (x y c rx ry rc)
                (smt:$exists (r)
                    (smt:and
                     (smt:$apply e.sem 1 x y c r)
                     (smt:= rx r)
                     (smt:= ry y)
                     (smt:= rc c))))
          ("y=" (x y c rx ry rc)
                (smt:$exists (r)
                    (smt:and
                     (smt:$apply e.sem 1 x y c r)
                     (smt:= rx x)
                     (smt:= ry r)
                     (smt:= rc c))))
          ("c=" (x y c rx ry rc)
                (smt:$exists (r)
                    (smt:and
                     (smt:$apply e.sem 1 x y c r)
                     (smt:= rx x)
                     (smt:= ry y)
                     (smt:= rc r))))
          (";" (x y c rx ry rc)
               (smt:$exists (r1 r2 r3)
                            (smt:and
                             (smt:$apply s.sem 1 x y c r1 r2 r3)
                             (smt:$apply s.sem 2 r1 r2 r3 rx ry rc))))
          ("eps" (x y c rx ry rc)
                 (smt:and (smt:= x rx) (smt:= y ry) (smt:= c rc)))
          ("site" (x y c rx ry rc)
                  (smt:$exists (b r1 r2) (smt:and
                                          (smt:$apply b.sem 1 x y c b)
                                          (smt:or
                                           (smt:and
                                            b
                                            (smt:$apply s.sem 2 x y c rx ry rc))
                                           (smt:and
                                            (smt:not b)
                                            (smt:$apply s.sem 3 x y c rx ry rc)))))))
   (E (e.sem Int Int Int Int Int)
          ("x" (x y c r) (smt:= r x))
          ("y" (x y c r) (smt:= r y))
          ("0" (x y c r) (smt:= r 0))
          ("1" (x y c r) (smt:= r 1))
          ("1+" (x y c r) (smt:$exists (r1) (smt:and
                                             (smt:$apply e.sem 1 x y c r1)
                                             (smt:= r (smt:+ r1 1))))))
   (B (b.sem Int Int Int Int Bool)
      ("true" (x y c b) (smt:= b (smt:$true)))
      ("false" (x y c b) (smt:= b (smt:$false)))
      ("not" (x y c b) (smt:$exists (b1) (smt:and
                                        (smt:$apply b.sem 1 x y c b1)
                                        (smt:not (smt:= b1 b)))))
      ("and" (x y c b) (smt:$exists (b1 b2) (smt:and
                                           (smt:$apply b.sem 1 x y c b1)
                                           (smt:$apply b.sem 2 x y c b2)
                                           (smt:= b (smt:and b1 b2)))))
      ("<" (x y c b) (smt:$exists (r1 r2) (smt:and
                                         (smt:$apply e.sem 1 x y c r1)
                                         (smt:$apply e.sem 2 x y c r2)
                                         (smt:= b (smt:< r1 r2))))))
   (Angel (b.sem Int Int Int Int Bool)
          ("<angelic>" (x y c b) (smt:$true)))))

(defparameter *plusy-impv-io* (make-instance 'f:frangel-problem
                                             :grammar *plusy-impv-g*
                                             :angelic-grammar *plusy-impv-g-a*
                                             :semantics *plusy-impv-sem*
                                             :specification (make-instance 'semgus:io-specification)))

(semgus:add-example (semgus:specification *plusy-impv-io*)
                    '((:x . 4) (:y . 0) (:c . 0))
                    '((:x . 4) (:y . 0) (:c . 0)))
(semgus:add-example (semgus:specification *plusy-impv-io*)
                    '((:x . 4) (:y . 1) (:c . 0))
                    '((:x . 4) (:y . 0) (:c . 4)))
(semgus:add-example (semgus:specification *plusy-impv-io*)
                    '((:x . 3) (:y . 2) (:c . 0))
                    '((:x . 3) (:y . 0) (:c . 6)))
(semgus:add-example (semgus:specification *plusy-impv-io*)
                    '((:x . 5) (:y . 4) (:c . 0))
                    '((:x . 5) (:y . 0) (:c . 20)))
(semgus:add-example (semgus:specification *plusy-impv-io*)
                    '((:x . 10) (:y . 10) (:c . 0))
                    '((:x . 10) (:y . 0) (:c . 100)))

'(defparameter *plusy-impv-rel*
  (make-instance 'f:frangel-problem
                 :grammar *max2-impv-g*
                 :angelic-grammar *max2-impv-g-a*
                 :semantics *max2-impv-sem*
                 :specification
                 (make-instance 'f:universal-formula
                                :inputs (list (smt:$int x) (smt:$int y) (smt:$int c))
                                :outputs (list (smt:$int rx) (smt:$int ry) (smt:$int rc))
                                :model-to-output-state #'(lambda (s)
                                                           (let ((c (copy-alist s)))
                                                             (rplaca (assoc :rx c) :x)
                                                             (rplaca (assoc :ry c) :y)
                                                             (rplaca (assoc :rc c) :c)
                                                             c))
                                :relation-application
                                (smt:$apply (smt:$function "rootprogram"
                                                (smt:*int-sort*
                                                 smt:*int-sort*
                                                 smt:*int-sort*
                                                 smt:*int-sort*
                                                 smt:*int-sort*
                                                 smt:*int-sort*)
                                                smt:*bool-sort*)
                                            (smt:$int x) (smt:$int y) (smt:$int c)
                                            (smt:$int rx) (smt:$int ry) (smt:$int rc))
                                :specification
                                (smt:and (smt:or (smt:and
                                                  (smt:< (smt:$int x) (smt:$int y))
                                                  (smt:= (smt:$int rc) (smt:$int y)))
                                                 (smt:and
                                                  (smt:< (smt:$int y) (smt:$int x))
                                                  (smt:= (smt:$int rc) (smt:$int x)))
                                                 (smt:and
                                                  (smt:= (smt:$int x) (smt:$int rc))
                                                  (smt:= (smt:$int y) (smt:$int rc))))
                                         (smt:= (smt:$int rx) 0)
                                         (smt:= (smt:$int ry) 0)))))
