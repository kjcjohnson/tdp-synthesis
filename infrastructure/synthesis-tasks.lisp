;;;;
;;;; Synthesis tasks - the basic infrastructure for TDP synthesis
;;;;
(in-package #:com.kjcjohnson.tdp)
(kl/oo:import-classes-from #:com.kjcjohnson.synthkit.vsa)

;;;
;;; The specialized synthesis task.
;;; Techniques should define a specialized method for this.
;;;
(defgeneric synthesize* (obj nt-or-prod info)
  (:documentation "Computes a set of terms rooted at NT described by INFO,
specialized for OBJ"))

;;;
;;; The specialized synthesis dispatch generic function.
;;; This should be specialized on the overall algorithm to choose a dispatcher
;;;
(defgeneric synthesize-dispatch (algo nt-or-prod info)
  (:documentation
   "Chooses a specialized synthesis task best for the given problem state, based 
on the algorithm state ALGO, non-terminal NT, and information INFO. 
Returns an object that will be passed to SYNTHESIZE*."))

;;;
;;; The recursive synthesis task. Call this to synthesize a new program set.
;;;
(defun synthesize (nt-or-prod info)
  "Synthesizes a set of terms rooted at NT that are described by INFO."
  (let ((specializer (synthesize-dispatch *algorithm* nt-or-prod info)))
    ;;(format *trace-output* "; SPEC: ~a~%" specializer)
    (if (typep nt-or-prod 'g:non-terminal)
        (with-depth-layer
          (synthesize* specializer nt-or-prod info))
        (with-next-production (nt-or-prod)
          (synthesize* specializer nt-or-prod info)))))

;;;
;;; Propagation functions
;;;
(defgeneric infer (prod child-index outer-spec context-info)
  (:documentation "Infers new information about child CHILD-INDEX of PROD from
CONTEXT-INFO. Note that CONTEXT-INFO includes initial information as well as info
about already-computed programs."))

(defgeneric derive (prod child-index program-set outer-spec context-info)
  (:documentation "Derives new information about a child of a production."))

;;;
;;; Combination function
;;;
(defgeneric combine (nt-or-prod children context-info))

;;;
;;; Generic functions for search strategies
;;;
(defgeneric get-search-strategy (algorithm nt-or-prod info)
  (:documentation
   "Returns a search strategy and initial state for the given nt or prod based 
on given information"))

(defgeneric is-search-done (nt-or-prod strategy state)
  (:documentation
   "Checks if the search is finished for the given strategy and state"))

(defgeneric get-next-search-query (nt-or-prod strategy state)
  (:documentation
   "Gets the next query in a search. Returns the next nt/prod, index, and 
context info to use"))

(defgeneric search-transition (nt-or-prod strategy state prog-set up-info)
  (:documentation
   "Gets the next state for the search"))

(defgeneric get-search-result (nt-or-prod strategy state)
  (:documentation
   "Gets the program set from the search result"))
