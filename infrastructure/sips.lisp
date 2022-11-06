;;;;
;;;; SIPS - sideways information passing strategies
;;;;
(in-package #:com.kjcjohnson.tdp)

(defclass sideways-information-passing-strategy ()
  ((ordering :reader sips.ordering)
   (dependencies :reader sips.dependencies)
   (arity :reader arity :initarg :arity))
  (:documentation "Holds information about how to pass information between
a production's children"))

(defun sips.ltr (arity)
  "Creates a default left-to-right SIPS for a production of the given arity."
  (let ((sips (make-instance 'sideways-information-passing-strategy :arity arity)))
    (setf (slot-value sips 'ordering)
          (loop for i from 0 below arity collecting i)
          (slot-value sips 'dependencies)
          (loop for i from 0 below arity
                collecting
                (loop for j from 0 below i collecting j)))
    sips))

(defgeneric sips.nth-child (n sips)
  (:documentation "Gets the nth child to consider")
  (:method (n (sips sideways-information-passing-strategy))
    (nth n (slot-value sips 'ordering))))

(defgeneric sips.nth-dependencies (n sips)
  (:documentation "Gets the depedencies for the nth (SIPS order) child")
  (:method (n (sips sideways-information-passing-strategy))
    (nth n (slot-value sips 'dependencies))))
