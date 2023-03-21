;;;;
;;;; Duet ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(register-solver :duet)

(define-solver-metadata :duet
  :name "Duet"
  :symbol "duet"
  :description "Combines bottom-up enumeration with top-down propagation"
  :action "Duet Solve"
  :spec-transformer #'%io-and-cegis-spec-transformer
  :options (list (make-solver-option :keyword :depth
                                     :name "depth"
                                     :description "Maximum depth to enumerate"
                                     :type :number
                                     :default nil)))

(defmethod solve-problem ((solver (eql :duet)) semgus-problem &key depth)
  (semgus:maybe-with-cegis (semgus-problem)
    (duet:duet-solve semgus-problem :depth depth)))
