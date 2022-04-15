;;;;
;;;; Propagation functions for enumerative solving
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defmethod tdp:infer ((prod g:production) child-ix (info enumerator-info))
  "Infers a new specification by decreasing the allowable depth."
  (let ((current-depth (slot-value info 'depth)))
    (if (> current-depth 1)
        (make-instance 'enumerator-info
                       :max-depth (1- (slot-value info 'depth))
                       :prune (slot-value info 'prune)
                       :inputs (slot-value info 'inputs))
        nil)))

(defmethod tdp:derive ((prod g:production) child-index prog-set (info enumerator-info))
  nil)

