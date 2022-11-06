;;;;
;;;; Entry point for the top-down enumerator
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defun top-down-enum-solve (semgus-problem)
  "Searches top-down for a satisfying program."
  (tdp::tdp-s semgus-problem
              (make-instance 'top-down-enum-algorithm)
              (make-instance 'initial-information
                             :specification
                             (semgus:specification semgus-problem))))
