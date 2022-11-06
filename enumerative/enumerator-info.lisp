;;;;
;;;; Information for enumerating
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defclass enumerator-info ()
  ((depth :initarg :max-depth)
   (prune :initarg :prune :initform nil)
   (inputs :initarg :inputs :reader inputs)
   (outputs :initarg :outputs :reader outputs)))

(defun make-enumerator-info (semgus-problem &key (max-depth 4) (prune t))
  "Creates enumerator info for a SemGuS problem"
  (make-instance 'enumerator-info
                 :max-depth max-depth
                 :prune prune
                 :inputs (map 'list
                              #'semgus:example-input
                               (semgus:examples 
                                (semgus:specification semgus-problem)))
                 :outputs (map 'list
                               #'semgus:example-output
                                (semgus:examples 
                                 (semgus:specification
                                  semgus-problem)))))

(defmethod print-object ((info enumerator-info) stream)
  "Prints the enumerator information"
  (print-unreadable-object (info stream :type t)
    (format stream "[d: ~s]" (slot-value info 'depth))))
