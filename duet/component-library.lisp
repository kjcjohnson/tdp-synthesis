;;;;
;;;; Component library synthesis task and infrastructure
;;;;
(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:kl/c)

;;;
;;; The component library
;;;
(kl/oo:define-encapsulated-class component-library
  :documentation "Enumerated library of components."
  (private field _components)

  (public constructor ()
          (setf _components (dictionary:new)))

  (public enumerate-to-depth (depth inputs descriptors)
          (let ((info (make-instance 'enum:enumerator-info
                                     :max-depth depth
                                     :prune t
                                     :inputs inputs
                                     :descriptors descriptors)))
            (kl:foreach (nt in (g:non-terminals tdp:*grammar*))
              (let ((comps (tdp:synthesize nt info)))
                (format *trace-output* "~&;;   Got ~a components for ~a~%"
                        (vsa:program-count comps) nt)
                (&dictionary:add _components
                                 nt
                                 comps)))))

  (public get-components (nt)
          (&dictionary:try-get-value _components nt)))


(defvar *component-library*)
(defvar *component-depth-override* 3)  ; depth = 4 causes OOM errors for now

(defmethod tdp:synthesize* ((obj (eql 'component-library-generate))
                            nt-or-prod
                            info)
  "Generates and uses a component library, a la Duet."
  (let ((*component-library* (component-library:new)))
    (loop for depth from 1 to *component-depth-override*
          for programs = (progn
                           (format t "; Enumerating to depth: ~a~%" depth)
                           (component-library:enumerate-to-depth
                            *component-library*
                            depth
                            (slot-value info 'inputs)
                            (slot-value info 'descriptors))
                           (run-reset-hooks tdp:*algorithm*)
                           (tdp:synthesize nt-or-prod info))
          while (zerop (vsa:program-count programs))
          finally (return programs))))


(defmethod tdp:synthesize* ((obj (eql 'component-library-task))
                            (nt g:non-terminal)
                            info)
  "Plucks out a set of enumerated components from the component library."
  (component-library:get-components *component-library* nt))

(defmethod tdp:synthesize* ((obj (eql 'check-component-library))
                            (nt g:non-terminal)
                            info)
  "Handles checking the library for our components."
  (vsa:do-programs (candidate (component-library:get-components
                             *component-library*
                             nt))
    (when (every #'(lambda (input output descriptor)
                     (smt:state= (ast:execute-program tdp:*semantics*
                                                      descriptor
                                                      candidate
                                                      input)
                                 output))
                 (slot-value info 'inputs)
                 (slot-value info 'outputs)
                 (slot-value info 'descriptors))
      (return-from tdp:synthesize* (make-instance 'vsa:leaf-program-node
                                                  :program candidate))))
  
  (let ((new-info (duet-information:copy info)))
    (setf (duet-information:check-library? new-info) nil)
    (let* ((res (tdp:synthesize nt new-info))
           (cnt (vsa:program-count res)))
      
      (values res cnt))))
                          
                          
