;;;;
;;;; Main Duet solver
;;;;
(in-package :com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:com.kjcjohnson.tdp.duet)

(defun duet-solve* (semgus-problem)
  "Implementation for solving a problem with the Duet solver"
  (assert (spec:is-pbe? (semgus:specification semgus-problem)))
  (let ((info (duet-information:new))
        (examples (spec:examples (semgus:specification semgus-problem))))
    (setf (duet-information:check-library? info)
          t)
    (setf (duet-information:inputs info)
          (map 'list #'spec:input-state examples))
    (setf (duet-information:outputs info)
          (map 'list #'spec:output-state examples))
    (setf (duet-information:descriptors info)
          (map 'list #'spec:descriptor examples))
    (tdp::tdp-s semgus-problem
                (make-instance 'duet-algorithm)
                info)))

(defun duet-solve (semgus-problem &key depth)
  "Solves a problem with the Duet solver"
  (if (null depth)
      (duet-solve* semgus-problem)
      (let ((*component-depth-override* depth))
        (duet-solve* semgus-problem))))
