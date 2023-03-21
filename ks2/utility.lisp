;;;;
;;;; ks2/solver utility functions
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(defun %io-and-cegis-spec-transformer (spec context)
  "Converts to either an IO spec, or a CEGIS spec"
  (if (spec:is-pbe? spec)
      spec
      (when (spec:cegis-supported-for-specification? spec context)
        (spec:convert-to-cegis spec))))

(defun %io-cegis-rel-spec-transformer (spec context)
  "Converts SPEC to an IO spec if possible, then CEGIS, but still returns relational
if cannot convert to either IO or CEGIS."
  (cond
    ((spec:is-pbe? spec)
     spec)
    ((spec:cegis-supported-for-specification? spec context)
     (spec:convert-to-cegis spec))
    (t
     spec)))
