;;;;
;;;; FrAngel ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

;;;
;;; FrAngel
;;;
(register-solver :frangel)

(defun frangel-spec-transformer (spec context)
  "Converts a specification into a form useful for FrAngel"
  (cond
    ((spec:is-pbe? spec)
     (make-instance 'spec:intersection-specification  ; We want a single top-level
                    :components (spec:examples spec))) ; intersection with examples
    ((%can-convert-to-cegis spec context)
     (spec:convert-to-cegis spec))
    (t
     spec)))

(define-solver-metadata :frangel
  :name "Fragment Search"
  :symbol "fragment-search"
  :description "Generates programs randomly, using fragments of partial programs"
  :action "Fragment Search"
  :spec-transformer #'frangel-spec-transformer
  :options (list))

(defmethod solve-problem ((solver (eql :frangel)) semgus-problem &key)
  (frangel:fragment-search semgus-problem))

;;;
;;; Random Search
;;;
(register-solver :random)

(define-solver-metadata :random
  :name "Random Search"
  :symbol "random"
  :description "Randomly generates and checks programs"
  :action "Random Search"
  :options (list))

(defmethod solve-problem ((solver (eql :random)) semgus-problem &key)
  (frangel:random-search semgus-problem))
