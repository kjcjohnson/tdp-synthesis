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

(defclass duet-refinement ()
  ((name :reader name
         :initarg :name
         :initform (error "Name is required."))
   (refinement-function :reader refinement-function
                        :initarg :refinement-function
                        :initform (error "Refinement function is required."))))

(defmethod kl:equals ((a duet-refinement) (b duet-refinement))
  "Checks if A and B are equal. Only the name is checked."
  (and (eql (name a) (name b))))

(defmethod kl:get-hash-code ((obj duet-refinement))
  "Gets a hash code for the refinement."
  (sxhash (name obj)))

(defmethod kl:equals ((a duet-information) (b duet-information))
  "Checks if A and B are equal"
  (and (kl:equals (duet-information:inputs a)
                  (duet-information:inputs b))
       (kl:equals (duet-information:outputs a)
                  (duet-information:outputs b))
       (kl:equals (duet-information:refinement a)
                  (duet-information:refinement b))
       (kl:equals (duet-information:check-library? a)
                  (duet-information:check-library? b))))

(defmethod kl:get-hash-code ((obj duet-information))
  "Gets a hash code for the given information."
  (logxor
   (sxhash (duet-information:inputs obj))
   (sxhash (duet-information:outputs obj))
   (kl:get-hash-code (duet-information:refinement obj))
   (sxhash (duet-information:check-library? obj))))

(defmethod print-object ((info duet-information) stream)
  "Prints a duet information object"
  (print-unreadable-object (info stream :type t)
    (format stream
            "~s ~~~~> ~s [LIB? ~s]"
            (duet-information:inputs info)
            (duet-information:outputs info)
            (duet-information:check-library? info))))
