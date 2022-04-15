;;;
;;; ring - a non-deterministic function
;;;
(in-package #:com.kjcjohnson.frangel.test)

(defun ring-g (&key angelic?)
  "Constructs the ring grammar."
  `(
    ("fc" Start fc)
    ("fx" Start fx)
    ("0" Start "0")
    ("1" Start "1")
    ("+" Start + Start Start)
    ("-" Start - Start)
    ("mux" Start mux Start Start)
    ;;("ite" Start ite ,(if angelic? 'Angel 'B) Start Start)
    ("true" B true)
    ("false" B false)
    ("not" B not B)
    ("and" B and B B)
    ,@(when angelic? '(("<angelic>" Angel angel)))
    ("<" B < Start Start)))
  
(defparameter *ring-g*
  (g:make-rtg :productions (ring-g :angelic? nil)))

(defparameter *ring-g-a*
  (g:make-rtg :productions (ring-g :angelic? t)))

(semgus:defsemantics *ring-sem*
                                        ; no operational semantics
  :operational nil
  :sorts ((Int smt:*int-sort*) (Bool smt:*bool-sort*))
  :variables ((fc Int) (fx Int) (r Int) (r1 Int) (r2 Int) (b Bool) (b1 Bool) (b2 Bool))
  :relational
  ((Start (s.sem Int Int Int Int)
          ("fc" (fc fx r) (smt:= r fc))
          ("fx" (fc fx r) (smt:= r fx))
          ("0" (fc fx r) (smt:= r 0))
          ("1" (fc fx r) (smt:= r 1))
          ("+" (fc fx r) (smt:$exists (r1 r2) (smt:and
                                             (smt:$apply s.sem 1 fc fx r1)
                                             (smt:$apply s.sem 2 fc fx r2)
                                             (smt:= r (smt:+ r1 r2)))))
          ("-" (fc fx r) (smt:$exists (r1) (smt:and
                                             (smt:$apply s.sem 1 fc fx r1)
                                             (smt:= r (smt:- 0 r1)))))
          ("mux" (fc fx r) (smt:$exists (r1 r2) (smt:and
                                                 (smt:$apply s.sem 1 fc fx r1)
                                                 (smt:$apply s.sem 2 fc fx r2)
                                                 (smt:or
                                                  (smt:= r r1)
                                                  (smt:= r r2)))))
          ("ite" (fc fx r) (smt:$exists (b r1 r2) (smt:and
                                                 (smt:$apply b.sem 1 fc fx b)
                                                 (smt:$apply s.sem 2 fc fx r1)
                                                 (smt:$apply s.sem 3 fc fx r2)
                                                 (smt:= r (smt:ite b r1 r2))))))
   (B (b.sem Int Int Int Bool)
      ("true" (fc fx b) (smt:= b (smt:$true)))
      ("false" (fc fx b) (smt:= b (smt:$false)))
      ("not" (fc fx b) (smt:$exists (b1) (smt:and
                                          (smt:$apply b.sem 1 fc fx b1)
                                          (smt:not (smt:= b1 b)))))
      ("and" (fc fx b) (smt:$exists (b1 b2) (smt:and
                                             (smt:$apply b.sem 1 fc fx b1)
                                             (smt:$apply b.sem 2 fc fx b2)
                                             (smt:= b (smt:and b1 b2)))))
      ("<" (fc fx b) (smt:$exists (r1 r2) (smt:and
                                           (smt:$apply s.sem 1 fc fx r1)
                                           (smt:$apply s.sem 2 fc fx r2)
                                           (smt:= b (smt:< r1 r2))))))
   (Angel (b.sem Int Int Int Bool)
          ("<angelic>" (fc fx b) (smt:$true)))))


(defparameter *ring-rel*
  (make-instance 'f:frangel-problem
                 :grammar *ring-g*
                 :angelic-grammar *ring-g-a*
                 :semantics *ring-sem*
                 :partials (list
                            (make-instance 'semgus:formula-specification
                                :formula
                                (smt:$exists ((smt:$int r))
                                   (smt:and
                                    (smt:$apply (smt:$function "rootprogram"
                                                    (smt:*int-sort*
                                                     smt:*int-sort*
                                                     smt:*int-sort*)
                                                    smt:*bool-sort*)
                                                (smt:$int fc) (smt:$int fx) (smt:$int r))
                                
                                    (smt:= (smt:$int r)
                                           (smt:-
                                            (smt:$int fc)
                                            (smt:$int fx))))))
                            
                            (make-instance 'semgus:formula-specification
                                :formula
                                (smt:$exists ((smt:$int r))
                                   (smt:and
                                    (smt:$apply (smt:$function "rootprogram"
                                                    (smt:*int-sort*
                                                     smt:*int-sort*
                                                     smt:*int-sort*)
                                                    smt:*bool-sort*)
                                                (smt:$int fc) (smt:$int fx) (smt:$int r))
                                
                                    (smt:= (smt:$int r)
                                           (smt:+
                                            (smt:$int fc)
                                            (smt:$int fx)))))))
                            
                 :specification
                 (make-instance 'f:universal-formula
                                :inputs (list (smt:$int fc) (smt:$int fx))
                                :outputs (list (smt:$int r))
                                :relation-application
                                (smt:$apply (smt:$function "rootprogram"
                                                (smt:*int-sort*
                                                 smt:*int-sort*
                                                 smt:*int-sort*)
                                                smt:*bool-sort*)
                                            (smt:$int fc) (smt:$int fx) (smt:$int r))
                                :specification
                               (smt:or
                                 (smt:= (smt:$int r)
                                        (smt:+
                                         (smt:$int fc)
                                         (smt:$int fx)))
                                 (smt:= (smt:$int r)
                                        (smt:-
                                         (smt:$int fc)
                                         (smt:$int fx)))))))
