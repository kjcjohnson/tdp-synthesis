;;;;
;;;; Combination functions for enumeration
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)
(kl/oo:import-classes-from #:vsa)
(kl/oo:import-classes-from #:kl/c)

(defmethod tdp:combine ((nt g:non-terminal) children (info enumerator-info))
  "Combines enumerators and optionally prunes based on obs. equivalence"
  '(format *trace-output*
          ";BP: ~a~%"
          (reduce #'+
                  (map 'list
                       #'(lambda (x) (program-node:program-count x))
                       children)))
           
  (let ((ret
          (if (not (slot-value info 'prune))
              (union-program-node:new children)
              (vsa:prune children (inputs info) tdp:*semantics*))))
    '(format *trace-output*
            ";AP: ~a~%"
            (program-node:program-count ret))
    ret))
    


(defmethod tdp:combine ((prod g:production) children (info enumerator-info))
  "Combines productions for enumeration"
  (if (zerop (g:arity prod))
      (leaf-program-node:new (make-instance 'ast:program-node :production prod))
      (cross-program-node:new prod children)))
