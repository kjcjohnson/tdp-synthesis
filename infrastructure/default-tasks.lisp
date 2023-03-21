;;;;
;;;; Default synthesis tasks for common situations
;;;;
(in-package #:com.kjcjohnson.tdp)

;;;
;;; Specialized task for returning an empty program node
;;;
(defmethod synthesize* ((obj (eql nil)) nt-or-prod info)
  (make-instance 'vsa:empty-program-node))

;;;
;;; Basic union combination
;;;
(defmethod combine ((nt g:non-terminal) children context-info)
  "Default non-terminal search combination."
  (make-instance 'vsa:union-program-node :programs children))

;;;
;;; Default non-terminal synthesis task
;;;
(defmethod synthesize* (obj (nt g:non-terminal) info)
  "The default non-terminal synthesis task."
  ;; TODO: search strategies for NTs
  (combine nt
           (map 'list
                #'(lambda (prod) (synthesize prod info))
                (g:productions-for-instance *grammar* nt))
           info))

;;;
;;; Default production synthesis task
;;;
(defmethod synthesize* (obj (prod g:production) info)
  (multiple-value-bind (strategy state) (get-search-strategy *algorithm* prod info)
    (loop until (is-search-done prod strategy state) doing
      (multiple-value-bind (nt ix context) (get-next-search-query prod strategy state)
        (let* ((down-info (infer prod ix info context))
               (synth-res (synthesize nt down-info))
               (up-info (derive prod ix synth-res info context)))
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
