;;;
;;; plusy-impv
;;;
(in-package #:com.kjcjohnson.frangel.test)

(defun dr-name-g ()
  "Generates the dr-name grammar."
  `(
    ("Start" Start "" ntString)
    ("name" ntString name)
    (" " ntString Space)
    ("." ntString Dot)
    ("Dr." ntString Dr.)
    ("concat" ntString str.++ ntString ntString)
    ("rplac" ntString str.replace ntString ntString ntString)
    ("charat" ntString str.at ntString ntInt)
    ("tostr" ntString int.to.str ntInt)
    ("substr" ntString str.substr ntString ntInt ntInt)

    ("0" ntInt "0")
    ("1" ntInt "1")
    ("2" ntInt "2")
    ("+" ntInt + ntInt ntInt)
    ("-" ntInt - ntInt ntInt)
    ("len" ntInt str.len ntString)
    ("toint" ntInt str.to.int ntString)
    ("indexof" ntInt str.indexof ntString ntString ntInt)
    ))

(defparameter *dr-name-g*
  (g:make-rtg :productions (dr-name-g)))

(semgus:defsemantics *dr-name-sem*
  :operational
  ((Start (is)
          ("Start" (sem1) ((funcall sem1 is))))
   (ntString (is)
             ("name" () ((cdr (assoc :name is))))
             (" " () ((declare (ignore is)) " "))
             ("." () ((declare (ignore is)) "."))
             ("Dr." () ((declare (ignore is)) "Dr."))
             ("concat" (sem1 sem2) ((concatenate 'string (funcall sem1 is) (funcall sem2 is))))
             ("rplac" (sem1 sem2 sem3) ((str:replace-first (funcall sem2 is)
                                                              (funcall sem3 is)
                                                              (funcall sem1 is))))
             ("charat" (sem1 sem2) ((let ((ix (funcall sem2 is))
                                          (str (funcall sem1 is)))
                                      (if (and (< ix (length str)) (>= ix 0))
                                          (string (char str ix))
                                          ""))))
             ("tostr" (sem1) ((princ-to-string (funcall sem1 is))))
             ("substr" (sem1 sem2 sem3) ((str:substring (funcall sem2 is)
                                                        (funcall sem3 is)
                                                        (funcall sem1 is)))))
   (ntInt (is)
          ("0" () ((declare (ignore is)) 0))
          ("1" () ((declare (ignore is)) 1))
          ("2" () ((declare (ignore is)) 2))
          ("+" (sem1 sem2) ((+ (funcall sem1 is) (funcall sem2 is))))
          ("-" (sem1 sem2) ((- (funcall sem1 is) (funcall sem2 is))))
          ("len" (sem1) ((length (funcall sem1 is))))
          ("toint" (sem1) ((or (parse-integer (funcall sem1 is) :junk-allowed t) -1)))
          ("indexof" (sem1 sem2 sem3) ((let ((start (funcall sem3 is))
                                             (main (funcall sem1 is)))
                                         (if (or (minusp start) (> start (length main)))
                                             -1
                                             (let ((val (search (funcall sem2 is)
                                                                main
                                                                :start2 start)))
                                               (if (null val)
                                                   -1
                                                   val))))))))
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

(defparameter *dr-name-io* (make-instance 'f:frangel-problem
                                             :grammar *dr-name-g*
                                             :angelic-grammar nil;*plusy-impv-g-a*
                                             :semantics *dr-name-sem*
                                             :specification (make-instance 'semgus:io-specification)))

(semgus:add-example (semgus:specification *dr-name-io*)
                    '((:name . "Nancy FreeHafer"))
                    "Dr. Nancy")
(semgus:add-example (semgus:specification *dr-name-io*)
                    '((:name . ""))
                    "Dr. ")
(semgus:add-example (semgus:specification *dr-name-io*)
                    '((:name . "Alfa"))
                    "Dr. Alfa")
(semgus:add-example (semgus:specification *dr-name-io*)
                    '((:name . "Mariya Sergienko"))
                    "Dr. Mariya")

;(semgus:add-example (semgus:specification *dr-name-io*)
;                    '((:name . "abcdefghijklmnopqrsatuvwxyz"))
;                    "19")

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
