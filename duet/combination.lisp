(in-package #:com.kjcjohnson.tdp.duet)

(defmethod tdp::combine ((prod g:production) children (info duet-information))
  "Default combination function."
  (if (zerop (g:arity prod))
      (let ((leaf (make-instance 'ast:program-node :production prod)))
        (if (every #'(lambda (input output)
                       (equal (ast:execute-program tdp:*semantics*
                                                   leaf
                                                   input)
                              output))
                   (duet-information:inputs info)
                   (duet-information:outputs info))
            (leaf-program-node:new leaf)
            (empty-program-node:new)))
      (cross-program-node:new prod children)))
