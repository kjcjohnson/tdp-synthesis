;;;;
;;;; Disjunctive information
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

(kl/oo:define-encapsulated-class disjunctive-duet-information
  :documentation "Holds multiple specifications"
  (public property sub-specifications)

  (public constructor (specs)
          (setf sub-specifications specs)))

(defmethod kl:equals ((a disjunctive-duet-information)
                      (b disjunctive-duet-information))
  "Checks if A and B are equal."
  (every #'kl:equals
         (disjunctive-duet-information:sub-specifications a)
         (disjunctive-duet-information:sub-specifications b)))

(defmethod kl:get-hash-code ((ddi disjunctive-duet-information))
  "Generates a hash code for the given disjunctive information."
  (reduce #'logxor
          (disjunctive-duet-information:sub-specifications ddi)
          :key #'kl:get-hash-code))

(defmethod print-object ((info disjunctive-duet-information) stream)
  "Prints a disjunctive spec."
  (print-unreadable-object (info stream :type t)
    (kl:foreach (spec in (disjunctive-duet-information:sub-specifications info))
      (format stream "~&| ~a~%" spec))))
