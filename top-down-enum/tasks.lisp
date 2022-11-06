;;;;
;;;; Synthesis tasks for top-down enumeration
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(kl/oo:import-classes-from #:vsa)

(defclass pq-entry ()
  ((program
    :initarg :program
    :reader program
    :documentation "Program for this queue entry.")
   (size
    :initarg :size
    :reader program-size
    :documentation "Size of the program entry.")
   (has-hole?
    :initarg :has-hole?
    :reader ast:has-hole?
    :documentation "Whether or not this entry has a hole.")))

(defmethod initialize-instance :after ((e pq-entry) &key)
  (unless (slot-boundp e 'program)
    (error "Program is required to create a pq-entry."))
  (unless (slot-boundp e 'size)
    (setf (slot-value e 'size) (ast:program-size (program e))))
  (unless (slot-boundp e 'has-hole?)
    (setf (slot-value e 'has-hole?) (ast:has-hole? (program e)))))

(defmethod tdp:synthesize* ((obj (eql 'top-down-initialize))
                            nt
                            (info initial-information))
  (let ((pq (priority-queue:make-pqueue #'<))
        (initial-nt (g:initial-non-terminal tdp:*grammar*))
        (timer (get-internal-real-time)))
    (priority-queue:pqueue-push
     (make-instance 'pq-entry
                    :program (make-instance 'ast:program-hole
                                            :non-terminal initial-nt)
                    :has-hole? t
                    :size 0)
     0
     pq)

    (loop for candidate = (priority-queue:pqueue-pop pq) doing
      (assert (ast:has-hole? candidate))
      (when (> (get-internal-real-time) timer)
        (incf timer (* internal-time-units-per-second 5))
        (format *trace-output* "~&; PQ Depth: ~a~%"
                (priority-queue:pqueue-length pq)))
      (let ((next (tdp:synthesize initial-nt
                                  (make-instance 'downward-information
                                                 :current-node
                                                 (program candidate)))))
        (assert (has-change? next))
        (dolist (pr (program-records next))
          (let ((size (ast:program-size (program pr))))
            (if (ast:has-hole? pr)
                (priority-queue:pqueue-push
                 (make-instance 'pq-entry
                                :program (program pr)
                                :has-hole? t
                                :size size)
                 size
                 pq)
                (when (every #'(lambda (ex)
                                 (smt:state=
                                  (ast:execute-program tdp:*semantics*
                                                       (program pr)
                                                       (semgus:example-input ex)) 
                                  (semgus:example-output ex)))
                             (semgus:examples (specification info)))
                  (format t "FOUND: [~a] ~a~%" size (program pr))
                  (return-from tdp:synthesize*
                    (leaf-program-node:new (program pr)))))))))))

;;;
;;; Hole-filling task for productions
;;;
(defmethod tdp:synthesize* ((obj (eql 'fill-hole))
                            (prod g:production)
                            (info downward-information))
  (if (zerop (g:arity prod))
      (make-instance 'program-record
                     :production prod
                     :has-hole? nil
                     :children nil)
      (make-instance 'program-record
                     :production prod
                     :has-hole? t
                     :children (map 'list
                                    #'(lambda (nt)
                                        (make-instance 'ast:program-hole
                                                       :non-terminal nt))
                                    (g:occurrences prod)))))
;;;
;;; Inferring. 
;;;
(defmethod tdp:infer ((prod g:production)
                      child-index
                      (outer-spec downward-information)
                      context-info)
  ;; Advance to the next non-terminal
  (if (typep (current-node outer-spec) 'ast:program-node)
      (make-instance 'downward-information
                     :current-node (ast:nth-child child-index
                                                  (current-node outer-spec)))
      outer-spec))
                                        ; We do this to make dispatch work
                     


(defmethod tdp:combine ((nt g:non-terminal)
                        children
                        (info downward-information))
  "Combines hole-filled productions...maybe"
  (if (typep (current-node info) 'ast:program-node)
      (progn
        (assert (= 1 (length children)))
        (first children))
      (make-instance 'upward-information
                     :has-change? t
                     :program-records children)))

(defmethod tdp:combine ((prod g:production)
                        children
                        (info downward-information))
  (if (zerop (g:arity prod))
      (make-instance 'ast:program-node :production prod)
      (break)))
      

#|
;;; Default LTR is good enough for this.

;;;
;;; Search strategy for filling non-terminal holes
;;;
(defclass nt-hole-fill-search-strategy () ())
(defparameter *nt-hole-fill-search-strategy*
  (make-instance 'nt-hole-fill-search-strategy))

(defmethod tdp:get-search-strategy ((algo top-down-enum-algorithm)
                                    (nt g:non-terminal)
                                    (info downward-information))
  (if (typep (current-node info) 'ast:program-node)
      (call-next-method)
      (values
       *nt-hole-fill-search-strategy*
       (g:productions nt))))

(defmethod is-search-done ((nt g:non-terminal)
                           (strat nt-hole-fill-search-strategy)
                           
|#

(defclass top-down-traverse-search-strategy () ())
(defparameter *top-down-traverse-search-strategy*
  (make-instance 'top-down-traverse-search-strategy))


(defmethod tdp:get-search-strategy ((algo top-down-enum-algorithm)
                                    (nt g:non-terminal)
                                    (info downward-information))
  (if (typep (current-node info) 'ast:program-node)
      (values
       *top-down-traverse-search-strategy*
       ;; Value: next prod, then nil when done
       (cons (ast:production (current-node info)) nil))
      (call-next-method))) ; Default to left-to-right for hole-filling

(defmethod tdp:is-search-done ((nt g:non-terminal)
                               (strategy top-down-traverse-search-strategy)
                               state)
  (null (car state)))

(defmethod tdp:get-next-search-query ((nt g:non-terminal)
                                      (strategy top-down-traverse-search-strategy)
                                      state)
  (values (car state) 0 nil))

(defmethod tdp:search-transition ((nt g:non-terminal)
                                  (strategy top-down-traverse-search-strategy)
                                  state
                                  prog-set
                                  up-info)
  (setf (car state) nil)
  (setf (cdr state) up-info)
  state)

(defmethod tdp:get-search-result ((nt g:non-terminal)
                                  (strategy top-down-traverse-search-strategy)
                                  state)
  (cdr state))

;;;
;;; Productions
;;;
(defclass top-down-enum-prod-traverse-strategy ()
  ((info :initarg :info)))

(defmethod tdp:get-search-strategy ((algo top-down-enum-algorithm)
                                    (prod g:production)
                                    (info downward-information))
  (if (typep (current-node info) 'ast:program-node)
      (values (make-instance 'top-down-enum-prod-traverse-strategy
                             :info info)
              (list nil 0 nil))
      (call-next-method)))

(defmethod tdp:is-search-done ((prod g:production)
                               (strategy top-down-enum-prod-traverse-strategy)
                               state)
  (or
   (third state)
   (>= (second state) (length (g:occurrences prod)))))

(defmethod tdp:get-next-search-query ((prod g:production)
                                      (strategy top-down-enum-prod-traverse-strategy)
                                      state)
  (values (nth (second state) (g:occurrences prod))
          (second state)
          (slot-value strategy 'info)))

(defmethod tdp:search-transition ((prod g:production)
                                  (strategy top-down-enum-prod-traverse-strategy)
                                  state
                                  prog-set
                                  up-info)
  (push prog-set (first state))
  (setf (second state) (1+ (second state)))
  (when (has-change? prog-set)
    (setf (third state) t)
    (loop for i from (second state) below (length (g:occurrences prod)) doing
      (push (ast:nth-child i (current-node (slot-value strategy 'info)))
            (first state))))
  state)

(defmethod tdp:get-search-result ((prod g:production)
                                  (strategy top-down-enum-prod-traverse-strategy)
                                  state)
  (assert (= (g:arity prod) (length (first state))))
  (upward-information/extend (slot-value strategy 'info) (nreverse (first state))))

(defmethod tdp:synthesize* ((obj (eql :tdp))
                            (nt g:non-terminal)
                            (info downward-information))
  (if (typep (current-node info) 'ast:program-node)
      (tdp:synthesize (ast:production (current-node info)) info)
      (call-next-method)))
