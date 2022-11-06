;;;
;;; plusy-impv
;;;
(in-package #:com.kjcjohnson.tdp.test)

(defun dr-name-g ()
  "Generates the dr-name grammar."
  `(
    ("Start" Start "" ntString)
    ("name" ntString name)
    (" " ntString Space)
    ("." ntString Dot)
    ("Dr." ntString Dr.)
    ("concat" ntString str.++ ntString ntString)
    ;; This one is slow!!!
    ;;("rplac" ntString str.replace ntString ntString ntString)
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
          ("+" (sem1 sem2) ((+ (the fixnum (funcall sem1 is))
                               (the fixnum (funcall sem2 is)))))
          ("-" (sem1 sem2) ((- (the fixnum (funcall sem1 is))
                               (the fixnum (funcall sem2 is)))))
          ("len" (sem1) ((length (the string (funcall sem1 is)))))
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
                                                   val)))))))))
(kl/oo:import-classes-from #:duet)

(defun dr-name-duet-info-easy ()
  "Creates the initial info for the Dr. name problem."
  (let ((info (duet-information:new)))
    (setf (duet-information:check-library? info) t)
    (setf (duet-information:outputs info) '("Dr. Nancy" "Dr. Alfa" "Dr. Mariya"))
    (setf (duet-information:inputs info) '(
                                           ((:name . "Nancy FreeHafer"))
                                           ((:name . "Alfa Beta"))
                                           ((:name . "Mariya Sergienko"))))
    info))

(defun dr-name-duet-info ()
  "Creates the initial info for the Dr. name problem."
  (let ((info (duet-information:new)))
    (setf (duet-information:check-library? info) t)
    (setf (duet-information:outputs info) '("Dr. Nancy" "Dr. Alfa" "Dr. Mariya"))
    (setf (duet-information:inputs info) '(
                                           ((:name . "Nancy FreeHafer"))
                                           ((:name . "Alfa"))
                                           ((:name . "Mariya Sergienko"))))
    info))

