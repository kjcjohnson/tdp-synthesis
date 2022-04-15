;;;;
;;;; Synthesis tasks - the basic infrastructure for TDP synthesis
;;;;
(in-package #:com.kjcjohnson.tdp)
(kl/oo:import-classes-from #:com.kjcjohnson.synthkit.vsa)

;;;
;;; The specialized synthesis task. Techniques should define a specialized method for this.
;;;
(defgeneric synthesize* (obj nt-or-prod info)
  (:documentation "Computes a set of terms rooted at NT described by INFO, specialized for OBJ"))

;;;
;;; The specialized synthesis dispatch generic function.
;;; This should be specialized on the overall algorithm to choose a dispatcher
;;;
(defgeneric synthesize-dispatch (algo nt-or-prod info)
  (:documentation
   "Chooses a specialized synthesis task best for the given problem state, based on the
algorithm state ALGO, non-terminal NT, and information INFO. 
Returns an object that will be passed to SYNTHESIZE*."))

;;;
;;; The recursive synthesis task. Call this to synthesize a new program set.
;;;
(defun synthesize (nt-or-prod info)
  "Synthesizes a set of terms rooted at NT that are described by INFO."
  (let ((specializer (synthesize-dispatch *algorithm* nt-or-prod info)))
    (synthesize* specializer nt-or-prod info)))

;;;
;;; Propagation functions
;;;
(defgeneric infer (prod child-index context-info)
  (:documentation "Infers new information about child CHILD-INDEX of PROD from CONTEXT-INFO.
Note that CONTEXT-INFO includes initial information as well as info about already-computed programs."))

(defgeneric derive (prod child-index program-set context-info)
  (:documentation "Derives new information about a child of a production."))

;;;
;;; Combination function
;;;
(defgeneric combine (nt-or-prod children context-info))

;;;
;;; Some default tasks
;;;
(defmethod synthesize* ((obj (eql nil)) nt-or-prod info)
  (empty-program-node:new))

(defmethod combine ((nt g:non-terminal) children context-info)
  "Default non-terminal search combination."
  (union-program-node:new children))

(defmethod synthesize* (obj (nt g:non-terminal) info)
  "The default non-terminal synthesis task."
  ;; TODO: search strategies for NTs
  (combine nt
           (map 'list
                #'(lambda (prod) (synthesize prod info))
                (g:productions-for-instance *grammar* nt))
           info))

;;;
;;; Generic functions for search strategies
;;;
(defgeneric get-search-strategy (algorithm nt-or-prod info)
  (:documentation
   "Returns a search strategy and initial state for the given nt or prod based on given information"))

(defgeneric is-search-done (nt-or-prod strategy state)
  (:documentation
   "Checks if the search is finished for the given strategy and state"))

(defgeneric get-next-search-query (nt-or-prod strategy state)
  (:documentation
   "Gets the next query in a search. Returns the next nt/prod, index, and context info to use"))

(defgeneric search-transition (nt-or-prod strategy state prog-set up-info)
  (:documentation
   "Gets the next state for the search"))

(defgeneric get-search-result (nt-or-prod strategy state)
  (:documentation
   "Gets the program set from the search result"))

;;;
;;; The default production synthesis task
;;;
;; (defmethod synthesize* (obj (prod g:production) info)
;;   "The default production synthesis task."
;;   ;; TODO: search strategies for productions
;;   ;; Current WRONG assumption is that we don't have dependencies between children
;;   (let ((children (loop for i from 0 below (g:arity prod)
;;                         collecting
;;                         (let ((nt (nth i (g:occurrences prod))))
;;                           (synthesize nt (infer prod i info))))))
;;     (combine prod children info)))

(defmethod synthesize* (obj (prod g:production) info)
  (multiple-value-bind (strategy state) (get-search-strategy *algorithm* prod info)
    (loop until (is-search-done prod strategy state) doing
      (multiple-value-bind (nt ix context) (get-next-search-query prod strategy state)
        (let* ((down-info (infer prod ix context))
               (synth-res (synthesize nt down-info))
               (up-info (derive prod ix synth-res context)))
          (setf state (search-transition prod strategy state synth-res up-info)))))
    (get-search-result prod strategy state)))

;;;
;;; Default left-to-right search
;;;
(defclass left-to-right-search-strategy ()
  ((info :initarg :info)))

(defmethod get-search-strategy (algorithm nt-or-prod info)
  (values (make-instance 'left-to-right-search-strategy :info info) (cons nil 0)))
  
(defmethod is-search-done ((prod g:production) (strategy left-to-right-search-strategy) state)
  (>= (cdr state) (length (g:occurrences prod))))

(defmethod get-next-search-query ((prod g:production) (strategy left-to-right-search-strategy) state)
  (values (nth (cdr state) (g:occurrences prod))
          (cdr state)
          (slot-value strategy 'info)))

(defmethod search-transition ((prod g:production)
                              (strategy left-to-right-search-strategy)
                              state
                              prog-set
                              up-info)
  (cons (append (car state) (list prog-set)) (1+ (cdr state))))

(defmethod get-search-result ((prod g:production) (strategy left-to-right-search-strategy) state)
  (combine prod (car state) (slot-value strategy 'info)))
          
