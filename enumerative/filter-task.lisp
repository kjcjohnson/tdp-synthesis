;;;;
;;;; Synthesis task for filtering enumerated programs
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)
(kl/oo:import-classes-from #:vsa)

(defmethod tdp:synthesize* ((obj (eql 'enumerative-filter-task))
                            nt-or-prod
                            (info enumerator-info))
  "Checks enumerated programs that satisfy a specification."
  (let* ((new-info (make-instance 'enumerator-info
                                  :max-depth (slot-value info 'depth)
                                  :prune (slot-value info 'prune)
                                  :inputs (slot-value info 'inputs)))
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
                          (equal (ast:execute-program tdp:*semantics*
                                                      candidate
                                                      (car exs))
                                 (cadr exs)))
                      (mapcar #'list (inputs info) (outputs info))))
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
    min-prog)))
