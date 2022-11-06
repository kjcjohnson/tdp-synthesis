;;;;
;;;; Top-down enumerator algorithm
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defclass top-down-enum-algorithm () ())

(defmethod tdp:synthesize-dispatch ((algorithm top-down-enum-algorithm)
                                    (nt g:non-terminal)
                                    (info downward-information))
  :tdp)

(defmethod tdp:synthesize-dispatch ((algorithm top-down-enum-algorithm)
                                    (prod g:production)
                                    (info downward-information))
  (etypecase (current-node info)
    (ast:program-node
     :tdp)
    (ast:program-hole
     'fill-hole)))

(defmethod tdp:synthesize-dispatch ((algorithm top-down-enum-algorithm)
                                    (nt g:non-terminal)
                                    (info initial-information))
  'top-down-initialize)
