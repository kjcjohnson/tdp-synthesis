;;;;
;;;; ks2/solver utility functions
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(defun %io-and-cegis-spec-transformer (spec context)
  "Converts to either an IO spec, or a CEGIS spec"
  (declare (ignore context))
  (if (and (spec:is-only-io? spec) (spec:with-only-intersection? spec))
      spec
      (spec:convert-to-cegis spec)))

