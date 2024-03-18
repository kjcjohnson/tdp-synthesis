;;;;
;;;; New program record implementation - hopefully better
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defstruct (shin-program-entry (:conc-name pe-))
  "New-style program entry on the queue"
  program size filled-trace hole-traces hole-nts)

(defun pe-has-hole? (pe)
  (declare (type shin-program-entry pe))
  (not (null (pe-hole-traces pe))))

(declaim (inline %copy-replacing-child))
(defun %copy-replacing-child (program index new-child)
  "Copies a program node and replaces a child"
  (make-instance 'ast:program-node
                 :production (ast:production program)
                 :operator (ast:operator program)
                 :children (let ((children (copy-list (ast:children program))))
                             (setf (elt children index) new-child)
                             children)))

(defun %generate-new-traces (current prod)
  "Generates new hole traces for PROD, based on CURRENT trace"
  (loop for ix from 0 below (g:arity prod)
        for new-arr = (make-array (1+ (length current)) :element-type 'fixnum)
        do (loop for i from 0 below (length current)
                 do (setf (aref new-arr i) (aref current i)))
           (setf (aref new-arr (length current)) ix)
        collect new-arr))

(defun get-initial-entry (grammar)
  "Generates the initial program entry"
  (make-shin-program-entry
   :program (make-instance 'ast:program-hole
                           :non-terminal (g:initial-non-terminal grammar))
   :size 1
   :hole-traces (list (make-array 0 :element-type 'fixnum))
   :hole-nts (list (g:initial-non-terminal grammar))))

(defun expand-next-hole (pe grammar)
  (declare (type shin-program-entry pe))
  (let ((next-trace (pop (pe-hole-traces pe)))
        (next-nt (pop (pe-hole-nts pe))))
    (labels ((replace-trace (node tix prod)
               (if (typep node 'ast:program-hole)
                   (if (zerop (g:arity prod))
                       (make-instance 'ast:program-node
                                      :production prod)
                       (make-instance 'ast:program-node
                                      :production prod
                                      :children (map 'list
                                                     #'(lambda (nt)
                                                         (make-instance
                                                          'ast:program-hole
                                                          :non-terminal nt))
                                                     (g:occurrences prod))))
                   (let ((child-ix (aref next-trace tix)))
                     (%copy-replacing-child node
                                            child-ix
                                            (replace-trace
                                             (ast:nth-child child-ix node)
                                             (1+ tix)
                                             prod))))))
      (loop for prod in (g:productions-for-instance grammar next-nt)
            for root = (replace-trace (pe-program pe) 0 prod)
            collecting (make-shin-program-entry
                        :program root
                        :size (+ (pe-size pe) (g:arity prod))
                        :hole-traces (nconc (%generate-new-traces next-trace prod)
                                            (pe-hole-traces pe))
                        :hole-nts (append (g:occurrences prod)
                                          (pe-hole-nts pe)))))))
