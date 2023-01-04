;;;;
;;;; Synthesis task for filtering enumerated programs
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)
(kl/oo:import-classes-from #:vsa)

(defun $debug-break (program)
  (when
      (string= "#<PROGRAM-NODE $eval($concat($char_1,$or($char_1,$char_2)))>"
               (format nil "~a" program))
    (break)))

(defmethod tdp:synthesize* ((obj (eql 'enumerative-filter-task))
                            nt-or-prod
                            (info enumerator-info))
  "Checks enumerated programs that satisfy a specification."
  (let* ((new-info (make-instance 'enumerator-info
                                  :max-depth (slot-value info 'depth)
                                  :prune (slot-value info 'prune)
                                  :inputs (slot-value info 'inputs)
                                  :descriptors (slot-value info 'descriptors)))
         (programs (tdp:synthesize nt-or-prod new-info)))

    (let (min-prog
          (next-checkpoint (+ 10 (get-universal-time)))
          (p-count 0)
          (a-count 0)
          (t-count (program-node:program-count programs)))
      (format t "~&Number of programs: ~a~%" t-count)
      (kl:foreach (candidate in programs)
        (when (and
               (or (null min-prog)
                   (< (ast:program-size candidate)
                      (ast:program-size min-prog)))
               (every #'(lambda (exs)
                          ;;($debug-break candidate)
                          (smt:state= (ast:execute-program tdp:*semantics*
                                                           (third exs)
                                                           candidate
                                                           (first exs))
                                      (second exs)))
                      (mapcar #'list
                              (inputs info)
                              (outputs info)
                              (descriptors info))))
          (setf min-prog candidate)
          (format t "~&Good [~s]: ~a~%" (ast:program-size candidate) candidate))
        (incf p-count)
        (incf a-count)
        (when (< next-checkpoint (get-universal-time))
          (format t "~&Rate: ~a programs/s [~a%]~%"
                  (float (/ p-count 10))
                  (float (/ a-count t-count)))
          (setf next-checkpoint (+ 10 (get-universal-time))
                p-count 0)))
      
      (if (null min-prog)
          (empty-program-node:new)
          (leaf-program-node:new min-prog)))))
