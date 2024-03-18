;;;;
;;;; Top-down enumerator algorithm
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defclass top-down-enum-algorithm ()
  ((use-shin-task :initarg :use-shin-task
                  :reader use-shin-task))
  (:default-initargs :use-shin-task nil))

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
  (if (use-shin-task algorithm)
      'top-down-new
      'top-down-initialize))
