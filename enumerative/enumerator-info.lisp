;;;;
;;;; Information for enumerating
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defclass enumerator-info ()
  ((depth :initarg :max-depth)
   (prune :initarg :prune :initform nil)
   (inputs :initarg :inputs :reader inputs)
   (outputs :initarg :outputs :reader outputs)
   (descriptors :initarg :descriptors :reader descriptors)))

(defun make-enumerator-info (semgus-problem &key (max-depth 4) (prune t))
  "Creates enumerator info for a SemGuS problem"
  (assert (spec:is-pbe? (semgus:specification semgus-problem)))
  (let ((examples (spec:examples (semgus:specification semgus-problem))))
    (make-instance 'enumerator-info
                   :max-depth max-depth
                   :prune prune
                   :inputs (map 'list #'spec:input-state examples)
                   :outputs (map 'list #'spec:output-state examples)
                   :descriptors (map 'list #'spec:descriptor examples))))

(defmethod print-object ((info enumerator-info) stream)
  "Prints the enumerator information"
  (print-unreadable-object (info stream :type t)
    (format stream "[d: ~s]" (slot-value info 'depth))))
