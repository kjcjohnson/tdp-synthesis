;;;;
;;;; FrAngel ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

;;;
;;; FrAngel
;;;
(register-solver :frangel)

(define-solver-metadata :frangel
  :name "Fragment Search"
  :symbol "fragment-search"
  :description "Generates programs randomly, using fragments of partial programs"
  :action "Fragment Search"
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
