(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:vsa)

(defmethod tdp:combine ((prod g:production) children (info duet-information))
  "Default combination function."
  (if (zerop (g:arity prod))
      (let ((leaf (make-instance 'ast:program-node :production prod)))
        (if (every #'(lambda (input output descriptor)
                       (smt:state= (ast:execute-program tdp:*semantics*
                                                        descriptor
                                                        leaf
                                                        input)
                                   output))
                   (duet-information:inputs info)
                   (duet-information:outputs info)
                   (duet-information:descriptors info))
            (leaf-program-node:new leaf)
            (empty-program-node:new)))
      (let ((descriptors (duet-information:descriptors info)))
        (vsa:prune
         (vsa:filter
          (cross-program-node:new prod children)
          (duet-information:inputs info)
          (duet-information:outputs info)
          tdp:*semantics*
          descriptors)
         (duet-information:inputs info)
         tdp:*semantics*
         descriptors
         :test #'kl:equals))))

(defmethod tdp:combine ((nt g:non-terminal) children (info duet-information))
  "Default non-terminal combination that prunes."
  (vsa:prune children
             (duet-information:inputs info)
             tdp:*semantics*
             (duet-information:descriptors info)
             :test #'kl:equals))
