;;;;
;;;; Duet, but with FrAngel
;;;;
(in-package #:com.kjcjohnson.tdp.test)
(kl/oo:import-classes-from #:duet)

(defclass frangel-duet-algorithm (duet::duet-algorithm)
  ((random-trials :initarg :random-trials :reader random-trials)
   (tried-random :initform nil :accessor tried-random)))

(defun frangel-duet-solve (semgus-problem &key (trials 20))
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
    (tdp::tdp-s semgus-problem
                (make-instance 'frangel-duet-algorithm :random-trials trials)
                info)))

(defmethod tdp::synthesize-dispatch ((algo frangel-duet-algorithm)
                                     (nt g:non-terminal)
                                     (info duet::duet-information))
  "Dispatches to the randomized task"
  ;(format *trace-output* "d[~a]" (tried-random algo))
  (if (tried-random algo)
      (progn
        (setf (tried-random algo) nil)
        (call-next-method))
      'frangel-duet))

(defmethod tdp:synthesize* ((obj (eql 'frangel-duet))
                            (nt g:non-terminal)
                            (info duet::duet-information))

  (let ((fspec (make-instance 'semgus:io-specification))
        (fgram (make-instance 'g:regular-tree-grammar
                              :productions (g::productions tdp:*grammar*)
                              :initial nt
                              :operators (g::operators tdp:*grammar*)
                              :non-terminals (g::non-terminals tdp:*grammar*))))
    (loop for input  in (duet-information:inputs info)
          for output in (duet-information:outputs info)
          do (semgus:add-example fspec input output))
    
    (let ((fproblem (make-instance 'frangel:frangel-problem
                                   :specification fspec
                                   :semantics tdp:*semantics*
                                   :grammar fgram))
          (fmanager (make-instance 'frangel::fragment-manager)))
      
      (let ((candidate (frangel::find-candidate-program fproblem
                                                        fmanager
                                                        :max-iter (random-trials tdp:*algorithm*))))
        (if (null candidate)
            (progn
              ;(format *trace-output* "; No random :(~%")
              (setf (tried-random tdp:*algorithm*) t)
              (tdp:synthesize nt info))
            (progn
              (format *trace-output* "; Got random candidate!~%")
              (make-instance 'vsa:leaf-program-node :program candidate)))))))
