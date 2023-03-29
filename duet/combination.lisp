(in-package #:com.kjcjohnson.tdp.duet)

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
            (vsa:make-leaf-program-node leaf)
            (vsa:make-empty-program-node)))
      (let ((descriptors (duet-information:descriptors info)))
        (vsa:prune
         (vsa:filter
          (make-instance 'vsa:cross-program-node :production prod :sets children)
          #'(lambda (candidate)
              (every #'(lambda (input output descriptor)
                         (smt:state= output (ast:execute-program tdp:*semantics*
                                                                 descriptor
                                                                 candidate
                                                                 input)))
                     (duet-information:inputs info)
                     (duet-information:outputs info)
                     (duet-information:descriptors info))))
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
             :test #'equal))
