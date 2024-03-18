;;;;
;;;; Synthesis tasks for top-down enumeration
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(u:declare-timed-section *enumerate-section* "enum stats")

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

(defparameter *should-prune-hook* (constantly t))

(defclass abstraction-prune-strategy ()
  ((abstract-spec :reader abstract-spec
                  :initarg :abstract-spec
                  :documentation "Specification containing only abstract examples")
   (skip-prods :reader skip-prods
               :initarg :skip-prods
               :documentation "Productions to skip pruning"))
  (:documentation "A strategy for abstract pruning"))

(defvar *no-prune-1* nil)
(defvar *no-prune-2* nil)
(defvar *defer-prune* t "Whether or not to defer pruning until dequeue")
(defvar *collect-prune-stats* t)
(defvar *remove-pruned* t "Whether or not to remove pruned programs. For overhead.")

(defun setup-prune-strategy ()
  "Sets up data used in abstraction, if available"
  (let ((abstraction (gethash :abstraction (semgus:metadata tdp:*semgus-problem*))))
    (when abstraction
      ;; Filter out only abstract examples
      (let* ((old-spec (semgus:specification tdp:*semgus-problem*))
             (new-spec (spec:filter-examples-by-descriptor
                        old-spec
                        :include abstraction))
             (skip-prods nil))
        (unless *no-prune-1*
          (push (list (smt:ensure-identifier "$concat")) skip-prods)
          (push (list (smt:ensure-identifier "$or")) skip-prods))
        (unless *no-prune-2*
          (push (list (smt:ensure-identifier "$phi")
                      (smt:ensure-identifier "$star"))
                skip-prods)
          (push (list (smt:ensure-identifier "$eps")
                      (smt:ensure-identifier "$star"))
                skip-prods))
        (make-instance 'abstraction-prune-strategy :abstract-spec new-spec
                                                   :skip-prods skip-prods)))))

(defun try-prune-candidate (program trace prune-strategy)
  "Attempts to prune the candidate program, returning a Boolean. If T, the program does
not have any valid ways to fill its holes and can be safely pruned."
  (flet ((maybe-do-prune ()
           (when prune-strategy
             (not
              (semgus:check-program tdp:*semgus-problem*
                                    program
                                    :on-unknown :valid
                                    :specification (abstract-spec prune-strategy))))))
    ;;(format t "TRACE: ~a~%" (map 'list (*:compose #'smt:identifier-string #'g:name)
    ;;                             trace))
    (incf ast:*prune-candidate-counter*)
    (when (and prune-strategy
               (funcall *should-prune-hook* program)
               (not (member (map 'list #'g:name trace)
                            (skip-prods prune-strategy)
                            :test #'(lambda (x y)
                                      (and (>= (length x) (length y))
                                           (every #'eql x y)))))
               (not (eql (first trace) (second trace))))
      (let ((pruned? (maybe-do-prune)))
        (when *collect-prune-stats* (mark-is-pruned program trace pruned?))
        (incf ast:*prune-attempt-counter*)
        (when pruned? (incf ast:*prune-success-counter*)
              #+()(format *trace-output* "++PRUNE++: ~%~a~%~%" program))
        pruned?))))

(defparameter *prune-stats-by-hole* (make-array 10 :adjustable t
                                                   :fill-pointer 0
                                                   :initial-element nil))
(defparameter *prune-stats-by-size* (make-array 10 :adjustable t
                                                   :fill-pointer 0
                                                   :initial-element nil))

(defparameter *prune-stats-by-production* (make-hash-table :test 'equal))

(defun reset-prune-stats ()
  "Resets the pruning statistic arrays"
  (setf *prune-stats-by-hole* (make-array 10 :adjustable t
                                             :fill-pointer 0
                                             :initial-element nil))
  (setf *prune-stats-by-size* (make-array 10 :adjustable t
                                             :fill-pointer 0
                                             :initial-element nil))
  (setf *prune-stats-by-production* (make-hash-table :test 'equal)))

(defun mark-is-pruned (program trace pruned?)
  "Marks if a partial program is pruned or not"
  (flet ((get-cell (vec ix)
           "Extends the vector VEC if needed and returns the cell at IX."
           (let ((max-length (first (array-dimensions vec))) ; Avoid fill pointer
                 (fill-pointer (fill-pointer vec)))
             (when (>= ix max-length)
               (adjust-array vec (* 2 max-length) :initial-element nil))
             (when (>= ix fill-pointer)
               (setf (fill-pointer vec) (1+ ix))))
           (unless (consp (aref vec ix))
             (setf (aref vec ix) (cons 0 0)))
           (aref vec ix))
         (get-#-cell (ht key)
           (multiple-value-bind (cell found?)
               (gethash key ht)
             (unless found?
               (setf cell (cons 0 0))
               (setf (gethash key ht) cell))
             cell))
         (trace-key ()
           "Gets a key from the trace"
           (case (length trace)
             (0 nil)
             (1 (smt:identifier-string (g:name (first trace))))
             (otherwise
              (str:concat
               (smt:identifier-string (g:name (second trace)))
               "/"
               (smt:identifier-string (g:name (first trace))))))))

    (let ((size-cell (get-cell *prune-stats-by-size* (ast:program-size program)))
          (hole-cell (get-cell *prune-stats-by-hole* (ast:hole-count program)))
          (prod-cell (get-#-cell *prune-stats-by-production* (trace-key))))
      (incf (cdr size-cell))
      (incf (cdr hole-cell))
      (incf (cdr prod-cell))
      (when pruned?
        (incf (car size-cell))
        (incf (car hole-cell))
        (incf (car prod-cell))))))

(defun report-prune-stats ()
  "Prints out statistics about pruning"
  (format t "Prune by Hole Count~%------------------~%")
  (loop for ix below (fill-pointer *prune-stats-by-hole*)
        for cell = (aref *prune-stats-by-hole* ix)
        if (null cell)
          do (format t "[~2d] <none>~%" ix)
        else
          do (format t "[~2d] ~d/~d (~,2f%)~%" ix (car cell) (cdr cell)
                     (* 100 (/ (car cell) (cdr cell)))))

  (format t "Prune by Program Size~%------------------~%")
  (loop for ix below (fill-pointer *prune-stats-by-size*)
        for cell = (aref *prune-stats-by-size* ix)
        if (null cell)
          do (format t "[~2d] <none>~%" ix)
        else
          do (format t "[~2d] ~d/~d (~,2f%)~%" ix (car cell) (cdr cell)
                     (* 100 (/ (car cell) (cdr cell)))))

  (format t "Prune by Production~%------------------~%")
  (loop for prod being the hash-keys of *prune-stats-by-production*
        for cell = (gethash prod *prune-stats-by-production*)
        if (null cell)
          do (format t "[~a] <none>~%" prod)
        else
          do (format t "[~a] ~d/~d (~,2f%)~%" prod
                     (car cell) (cdr cell)
                     (* 100 (/ (car cell) (cdr cell))))))

(defvar *use-new-pq* nil)
(defvar *pq-impl* nil)
(defvar *initial-queue-size* nil)

(defmethod tdp:synthesize* ((obj (eql 'top-down-initialize))
                            nt
                            (info initial-information))
  (format t "~&-----~%Defer Prune: ~a~%Remove Pruned: ~a~%-----~%"
          *defer-prune* *remove-pruned*)
  (when *collect-prune-stats* (reset-prune-stats))
  (let ((pq (if *pq-impl*
                (make-queue *pq-impl* *initial-queue-size*)
                (if *use-new-pq*
                    (make-instance 'program-queue)
                    (make-instance 'pq-program-queue))))
        (initial-nt (g:initial-non-terminal tdp:*grammar*))
        (timer (get-internal-real-time))
        (prune-strategy (setup-prune-strategy)))
    (enqueue-program pq
                     (make-instance 'ast:program-hole :non-terminal initial-nt)
                     0
                     t
                     nil)

    (loop
      (multiple-value-bind (candidate size has-hole? trace)
          (dequeue-program pq)
        (declare (ignore size))
        (assert has-hole?)
        (when (> (get-internal-real-time) timer)
          (incf timer (* internal-time-units-per-second 5))
          (format *trace-output* "~&; PQ Depth: ~a~%" (depth pq)))
        (unless (and *defer-prune*
                     (try-prune-candidate candidate trace prune-strategy)
                     *remove-pruned*)
          (let ((next (u:with-timed-section (*enumerate-section*)
                        (tdp:synthesize initial-nt
                                        (make-instance 'downward-information
                                                       :current-node
                                                       candidate)))))
            (assert (has-change? next))
            (dolist (pr (program-records next))
              (let ((size (ast:program-size (program pr))))
                (if (ast:has-hole? pr)
                    (progn
                      (incf ast:*candidate-partial-programs*)
                      (if (and (not *defer-prune*)
                               (try-prune-candidate (program pr) (hole-trace pr)
                                                    prune-strategy)
                               *remove-pruned*)
                          nil
                          (enqueue-program pq
                                           (program pr)
                                           size
                                           t
                                           (hole-trace pr))))
                  (progn
                    (incf ast:*candidate-concrete-programs*)
                    (when (= 1 (incf (getf ast:*concrete-candidates-by-size* size 0)))
                      (ast:add-checkpoint size))
                    (ast:trace-program (program pr))
                    (when (semgus:check-program tdp:*semgus-problem* (program pr))
                      (format t "; FOUND: [~a] ~a~%" size (program pr))
                      (when *collect-prune-stats* (report-prune-stats))
                      (format t "~&; ENUM: TIME: ~,2fs; GC: ~,2fs; ALLOC: ~,3f MiB~%"
                              (u:get-timed-section-real-time *enumerate-section*)
                              (u:get-timed-section-gc-time *enumerate-section*)
                              (u:get-timed-section-bytes-consed *enumerate-section*
                                                                :unit "MiB"))
                      (return-from tdp:synthesize*
                        (make-instance 'vsa:leaf-program-node
                                       :program (program pr))))))))))))))

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
      (let* ((has-hole? t)
             (children (map 'list
                            #'(lambda (nt)
                                (let ((prods
                                        (g:productions-for-instance
                                         (semgus:grammar tdp:*semgus-problem*)
                                         nt)))
                                  (if (= 1 (length prods))
                                      (let ((subprog (tdp:synthesize (first prods)
                                                                     info)))
                                        (unless (ast:has-hole? subprog)
                                          (setf has-hole? nil))
                                        (program subprog))
                                      (make-instance 'ast:program-hole
                                                     :non-terminal nt))))
                            (g:occurrences prod))))
        (make-instance 'program-record
                       :production prod
                       :has-hole? has-hole?
                       :children children))))
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
