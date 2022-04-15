;;;;
;;;; Complete enumerative algorithm
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defclass enumerator-algorithm () ())

(defmethod tdp:synthesize-dispatch ((algorithm enumerator-algorithm) nt-or-prod info)
  (cond ((null info)
         nil)
        ((string= (g:name nt-or-prod) "Constant")
         nil) ; Cannot enumerate constants
        ((and (slot-boundp info 'inputs)
              (slot-boundp info 'outputs))
         'enumerative-filter-task)
        (t
         :tdp)))
