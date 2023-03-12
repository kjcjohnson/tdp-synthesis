;;;;
;;;; ks2/solver utility functions
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(defun %can-convert-to-cegis (spec context)
  "Checks if SPEC can be converted to a CEGIS specification"
  (declare (ignore context))
  (or (spec:is-universal? spec) (spec:is-existential? spec)))

(defun %io-and-cegis-spec-transformer (spec context)
  "Converts to either an IO spec, or a CEGIS spec"
  (if (spec:is-pbe? spec)
      spec
      (when (%can-convert-to-cegis spec context)
        (spec:convert-to-cegis spec))))

(defun %io-cegis-rel-spec-transformer (spec context)
  "Converts SPEC to an IO spec if possible, then CEGIS, but still returns relational
if cannot convert to either IO or CEGIS."
  (cond
    ((spec:is-pbe? spec)
     spec)
    ((%can-convert-to-cegis spec context)
     (spec:convert-to-cegis spec))
    (t
     spec)))
