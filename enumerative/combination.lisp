;;;;
;;;; Combination functions for enumeration
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)
(kl/oo:import-classes-from #:kl/c)

(defmethod tdp:combine ((nt g:non-terminal) children (info enumerator-info))
  "Combines enumerators and optionally prunes based on obs. equivalence"
  '(format *trace-output*
          ";BP: ~a~%"
          (reduce #'+
                  (map 'list
                       #'(lambda (x) (vsa:program-count x))
                       children)))
  (let ((ret
          (if (not (slot-value info 'prune))
              (make-instance 'vsa:union-program-node :programs children)
              (vsa:prune children
                         (inputs info)
                         tdp:*semantics*
                         (descriptors info)))))
    '(format *trace-output*
            ";AP: ~a~%"
            (vsa:program-count ret))
    ret))
    


(defmethod tdp:combine ((prod g:production) children (info enumerator-info))
  "Combines productions for enumeration"
  (if (zerop (g:arity prod))
      (make-instance 'vsa:leaf-program-node
                     :program (make-instance 'ast:program-node :production prod))
      (make-instance 'vsa:cross-program-node
                     :production prod
                     :sets children)))
