;;;;
;;;; Entry point for the top-down enumerator
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defun top-down-enum-solve (semgus-problem &key shin-task)
  "Searches top-down for a satisfying program."
  (tdp::tdp-s semgus-problem
              (make-instance 'top-down-enum-algorithm :use-shin-task shin-task)
              (make-instance 'initial-information
                             :specification
                             (semgus:specification semgus-problem))))
