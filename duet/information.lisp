;;;;
;;;; Duet information
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

(kl/oo:define-encapsulated-class duet-information
  :documentation "Specification for a Duet problem."
  (public property inputs)
  (public property outputs)
  (public property descriptors)
  (public property refinement)
  (public property check-library?)

  (public constructor ()
          (setf inputs nil
                outputs nil
                descriptors nil
                refinement nil
                check-library? t))
  
  (public copy ()
          (let ((other (make-instance 'duet-information)))
            (setf (kl/oo:property-invoke other :inputs) inputs
                  (kl/oo:property-invoke other :outputs) outputs
                  (kl/oo:property-invoke other :descriptors) descriptors
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

(defun refinement= (a b)
  "Checks if A and B are equal. Only the name is checked."
  (declare (type (or duet-refinement null) a b))
  (cond
    ((and (null a) (null b))
     t)
    ((or (null a) (null b))
     nil)
    (t
     (and (eql (name a) (name b))))))

(defun refinement-hash-code (obj)
  "Gets a hash code for the refinement."
  (declare (type (or duet-refinement null) obj))
  (if (null obj)
      (sxhash nil)
      (sxhash (name obj))))

(defun information= (a b)
  "Checks if A and B are equal"
  (declare (type duet-information a b))
  (and
   ;; List of SMT states
   (every #'smt:state=
          (duet-information:inputs a)
          (duet-information:inputs b))
   ;; List of SMT states
   (every #'smt:state=
          (duet-information:outputs a)
          (duet-information:outputs b))
   ;; List of symbols
   (equal (duet-information:descriptors a)
          (duet-information:descriptors b))
   ;; Refinement object
   (refinement= (duet-information:refinement a)
                (duet-information:refinement b))
   ;; Boolean flag
   (equal (duet-information:check-library? a)
          (duet-information:check-library? b))))

(defun information-hash-code (obj)
  "Gets a hash code for the given information."
  (declare (type duet-information obj))
  (logxor
   (reduce #'logxor (duet-information:inputs obj) :key #'smt:state-hash-code)
   (reduce #'logxor (duet-information:outputs obj) :key #'smt:state-hash-code)
   (sxhash (duet-information:descriptors obj))
   (refinement-hash-code (duet-information:refinement obj))
   (sxhash (duet-information:check-library? obj))))

(defmethod print-object ((info duet-information) stream)
  "Prints a duet information object"
  (print-unreadable-object (info stream :type t)
    (format stream
            "~s ~~~~> ~s {~s} [LIB? ~s]"
            (duet-information:inputs info)
            (duet-information:outputs info)
            (duet-information:descriptors info)
            (duet-information:check-library? info))))
