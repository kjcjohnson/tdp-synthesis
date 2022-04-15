;;;;
;;;; The Duet dispatcher
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

(defmethod tdp:synthesize-dispatch ((algo duet-algorithm) nt-or-prod (info duet-information))
  "Dispatcher for Duet synthesis problems."
  (cond
    ((null info)
     nil)
    
    ((or (not (boundp '*component-library*))
         (null *component-library*))
     'component-library-generate)

    ((and (typep nt-or-prod 'g:non-terminal)
          (slot-value info 'check-library?))
     'check-component-library)

    (t
     :tdp)))


(defmethod tdp:synthesize-dispatch ((algorithm duet-algorithm) nt-or-prod (info enum::enumerator-info))
  (cond ((null info)
         nil)
        ((string= (g:name nt-or-prod) "Constant")
         nil) ; Cannot enumerate constants
        ((and (slot-boundp info 'enum::inputs)
              (slot-boundp info 'enum::outputs))
         'enumerative-filter-task)
        (t
         :tdp)))
