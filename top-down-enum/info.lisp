;;;;
;;;; Top-down enumerator information
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defclass downward-information ()
  ((current-node
    :initarg :current-node
    :reader current-node
    :documentation "Current program node under consideration")))

(defmethod print-object ((object downward-information) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (current-node object))))

(defclass initial-information ()
  ((specification
    :initarg :specification
    :reader specification
    :documentation "Specification for the desired program.")))

(defclass program-record ()
  ((program
    :initarg :program
    :reader program
    :documentation "Child program coming upwards")
   (has-hole?
    :initarg :has-hole?
    :reader ast:has-hole?
    :documentation "Whether or not this program has a hole")
   (trace
    :initarg :trace
    :reader hole-trace
    :documentation "Trace of productions from the filled hole to the root")))

(defmethod initialize-instance :after ((pr program-record)
                                       &key
                                         production
                                         (holiness nil holiness-supplied?)
                                         children)
  "Initializes a program record from a production and children."
  (unless (slot-boundp pr 'has-hole?)
    (unless holiness-supplied?
      (setf holiness (some #'ast:has-hole? children)))
    (when (consp holiness)
      (setf holiness (some #'identity holiness)))
    (setf (slot-value pr 'has-hole?) holiness))

  (unless (slot-boundp pr 'trace)
    (setf (slot-value pr 'trace) (list production)))

  (unless (slot-boundp pr 'program)
    (setf (slot-value pr 'program)
          (make-instance 'ast:program-node
                         :production production
                         :children children))))

(defclass upward-information ()
  ((program-records
    :initarg :program-records
    :reader program-records
    :documentation "List of programs coming up from the synthesis query.")
   (has-change?
    :initarg :has-change?
    :reader has-change?
    :documentation "Whether or not the upcoming (singleton) program has changed
from the program being traversed. Set only if holes have been filled out.")))

(defun upward-information/no-change (program)
  "Creates an upward information object with an unchanged program record."
  (make-instance 'upward-information
                 :has-change? nil
                 :program-records (list (make-instance 'program-record
                                                       :program program
                                                       :has-hole? nil))))

(defun upward-information/extend (downward-info children)
  "Creates an upward information object from a production and children. CHILDREN
can be either program nodes or upward information."
  ;; Invariant: only one child should be a changed program record
  (flet ((is-changed? (child)
           "Checks if a child is changed."
           (etypecase child
             (upward-information
              (has-change? child))
             ((or ast:program-node ast:program-hole)
              nil)))
         (get-program-list (child)
           "Gets the list of programs represented by this child."
           (etypecase child
             (upward-information
              (map 'list #'program (program-records child)))
             ((or ast:program-node ast:program-hole)
              (list child))))
         (get-holiness-list (child)
           "Gets a list of holiness of this child."
           (etypecase child
             (upward-information
              (map 'list #'ast:has-hole? (program-records child)))
             (ast:program-atom
              (list (ast:has-hole? child)))))
         (get-trace-list (child)
           "Gets traces"
           (etypecase child
             (upward-information
              (map 'list #'hole-trace (program-records child)))
             (ast:program-atom
              (list nil)))))
    (case (count t children :key #'is-changed?)
      (0 ;; No changes to any children
       (upward-information/no-change (current-node downward-info)))

      (1 ;; A single change
       (let* ((child-programs (map 'list #'get-program-list children))
              (all-child-programs (tdp:all-cart-prod child-programs))
              (child-holiness (map 'list #'get-holiness-list children))
              (all-child-holiness (tdp:all-cart-prod child-holiness))
              (child-traces (map 'list #'get-trace-list children))
              (all-child-traces (tdp:all-cart-prod child-traces))
              (current-production (ast:production (current-node downward-info))))
         (make-instance 'upward-information
                        :has-change? t
                        :program-records
                        (map 'list #'(lambda (children holiness traces)
                                       (when (< 1 (count t traces
                                                         :key (*:compose
                                                               #'*:true
                                                               #'car)))
                                         (break))
                                       (make-instance 'program-record
                                                      :holiness holiness
                                                      :trace (append
                                                              (find t traces
                                                                    :key (*:compose
                                                                          #'*:true
                                                                          #'car))
                                                              (list
                                                               current-production))
                                                      :production current-production
                                                      :children children))
                             all-child-programs
                             all-child-holiness
                             all-child-traces))))

      (otherwise
       (error "Multiple changed children when extending upward information")))))
