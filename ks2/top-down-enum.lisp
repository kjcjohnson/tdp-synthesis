;;;;
;;;; Top-down enumerator ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(register-solver :top-down-enum)

(define-solver-metadata :top-down-enum
  :name "Top-down Enumerator"
  :symbol "top-down-enum"
  :description "A standard top-down enumerative solver"
  :action "TDE Solve"
  :spec-transformer #'%io-cegis-rel-spec-transformer
  :options (list))

(defmethod solve-problem ((solver (eql :top-down-enum)) semgus-problem &key)
  (tde:top-down-enum-solve semgus-problem))
