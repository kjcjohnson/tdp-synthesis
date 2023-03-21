;;;;
;;;; Random Duet - Duet with a randomized component library
;;;;
(in-package #:com.kjcjohnson.tdp.test)
(kl/oo:import-classes-from #:duet)

(defclass random-duet-algorithm (duet::duet-algorithm)
  ((enum-switch :initform nil)))

(defun random-duet-solve (semgus-problem)
  "Solves a problem with the Duet solver"
  (let ((info (duet-information:new)))
    (setf (duet-information:check-library? info)
          t)
    (setf (duet-information:inputs info)
          (map 'list
               #'semgus:example-input
               (semgus:examples
                (semgus:specification semgus-problem))))
    (setf (duet-information:outputs info)
          (map 'list
               #'semgus:example-output
               (semgus:examples
                (semgus:specification semgus-problem))))
    (let ((duet::*component-depth-override* 50))
      (tdp::tdp-s semgus-problem
                  (make-instance 'random-duet-algorithm)
                  info))))

(defparameter *random-duet-randomness* 100 "From 0 to 100 on randomness")

(defmethod tdp:synthesize-dispatch ((algo random-duet-algorithm)
                                    (nt g:non-terminal)
                                    (info enum::enumerator-info))
  "Hijacks the Duet code and replaces the enumerator"
  (if (slot-value algo 'enum-switch)
      (call-next-method)
      'random-enumerate))

(defmethod tdp:synthesize* ((obj (eql 'random-enumerate))
                            (nt g:non-terminal)
                            (info enum::enumerator-info))
  (let ((kept-programs nil)
        (enumerated-programs
          (unwind-protect
               (progn
                 (setf (slot-value tdp:*algorithm* 'enum-switch) t)
                 (tdp:synthesize nt info))
            (setf (slot-value tdp:*algorithm* 'enum-switch) nil))))
    (kl:foreach (program in enumerated-programs)
      (when (> (random 100) *random-duet-randomness*)
        (push (make-instance 'vsa:leaf-program-node :program program) kept-programs)))
    (format *trace-output*
            "; Kept ~a enumerated programs~%"
            (length kept-programs))
    
    (let ((max-depth (1+ (slot-value info 'enum::depth))))
      (declare (type (integer 0 100) max-depth))
      (let ((rand-programs
              (loop for i from 0 to (* 10 (+ (floor (/ max-depth 4)) 1))
                    when (< (random 100) *random-duet-randomness*)
                      collect
                      (make-instance 'vsa:leaf-program-node
                                     :program
                                     (frangel::generate-random-tree tdp:*grammar*
                                                                    nil
                                                                    nt
                                                                    max-depth)))))

        (vsa:prune
         (make-instance
          'vsa:union-program-node
          :programs (list
                     (make-instance 'vsa:union-program-node :programs kept-programs)
                     (make-instance 'vsa:union-program-node :programs rand-programs)))
         (enum::inputs info)
         tdp:*semantics*
         (enum::descriptors info))))))

