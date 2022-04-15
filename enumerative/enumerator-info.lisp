;;;;
;;;; Information for enumerating
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defclass enumerator-info ()
  ((depth :initarg :max-depth)
   (prune :initarg :prune :initform nil)
   (inputs :initarg :inputs :reader inputs)
   (outputs :initarg :outputs :reader outputs)))
