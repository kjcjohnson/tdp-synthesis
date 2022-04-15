;;;;
;;;; Combination functions for enumeration
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)
(kl/oo:import-classes-from #:vsa)
(kl/oo:import-classes-from #:kl/c)

(defmethod tdp:combine ((nt g:non-terminal) children (info enumerator-info))
  "Combines enumerators and optionally prunes based on obs. equivalence"
  (if (not (slot-value info 'prune))
      (union-program-node:new children)
      (progn
        ;; Compute observational equivalence
        (let ((distinct (dictionary:new :test #'equal)))
          (kl:foreach (child in children)
            (kl:foreach (candidate in child)
              (let ((outputs (map 'list #'(lambda (input)
                                            (ast:execute-program tdp:*semantics*
                                                                 candidate
                                                                 input))
                                  (slot-value info 'inputs))))
                (if (not (&dictionary:contains-key distinct outputs))
                    (&dictionary:add distinct outputs candidate)
                    (when
                        (< (ast:program-size candidate)
                           (ast:program-size (&dictionary:try-get-value
                                              distinct
                                              outputs)))
                      (&dictionary:add distinct outputs candidate))))))
          (union-program-node:new (map 'list
                                       #'(lambda (p)
                                           (leaf-program-node:new p))
                                       (&dictionary:value-list distinct)))))))


(defmethod tdp:combine ((prod g:production) children (info enumerator-info))
  "Combines productions for enumeration"
  (if (zerop (g:arity prod))
      (leaf-program-node:new (make-instance 'ast:program-node :production prod))
      (cross-program-node:new prod children)))
