;;;;
;;;; Bottom-up enumerator ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(register-solver :bottom-up-enum)

(define-solver-metadata :bottom-up-enum
  :name "Bottom-up Enumerator"
  :symbol "bottom-up-enum"
  :description "A depth-bounded depth-first bottom-up enumerative solver"
  :action "Bottom-up Solve"
  :options (list
            (make-solver-option :keyword :max-depth
                                :name "max-depth"
                                :description "Maximum depth to enumerate"
                                :type :number)
            (make-solver-option :keyword :prune
                                :name "prune"
                                :description "Whether or not to prune redundant terms. Not necessarily safe for imperative programs."
                                :type :boolean)))

(defmethod solve-problem ((solver (eql :bottom-up-enum)) semgus-problem
                          &rest options &key max-depth prune)
  (declare (ignore max-depth prune))
  (apply #'enum:enum-solve semgus-problem options))
