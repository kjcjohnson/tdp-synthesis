;;;;
;;;; Duet information
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

(kl/oo:define-encapsulated-class duet-information
  :documentation "Specification for a Duet problem."
  (public property inputs)
  (public property outputs)
  (public property refinement)
  (public property check-library?)

  (public constructor ()
          (setf inputs nil
                outputs nil
                refinement nil
                check-library? t))
  
  (public copy ()
          (let ((other (make-instance 'duet-information)))
            (setf (kl/oo:property-invoke other :inputs) inputs
                  (kl/oo:property-invoke other :outputs) outputs
                  (kl/oo:property-invoke other :refinement) refinement
                  (kl/oo:property-invoke other :check-library?) check-library?)
            other)))
