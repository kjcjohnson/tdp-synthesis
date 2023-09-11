;;;;
;;;; Top-down queue, optimized for top-down enumeration by size
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

;;;
;;; The queue protocol
;;;
(defgeneric enqueue-program (queue program size has-hole? trace)
  (:documentation "Enqueues a program"))

(defgeneric dequeue-program (queue)
  (:documentation "Dequeues a program"))

(defgeneric depth (queue)
  (:documentation "Gets the queue depth"))

(defgeneric make-queue (designator initial-size)
  (:documentation "Makes a queue associated with DESIGNATOR and INITIAL-SIZE"))

;;;
;;; Queue nodes
;;;
(defstruct (program-queue-node
            (:constructor %make-program-queue-node
                (program size has-hole? trace)))
  program size has-hole? trace next)

;;;
;;; Default queue implementation
;;;
(defclass pq-program-queue ()
  ((pq :initarg :pq :initform (priority-queue:make-pqueue #'<))))

(defmethod make-queue ((designator (eql :pq)) initial-size)
  (declare (ignore initial-size))
  (make-instance 'pq-program-queue))

(declaim (inline pq-unwrap))
(defun pq-unwrap (pq-wrapper)
  "Unwraps a pq wrapper"
  (slot-value pq-wrapper 'pq))

(defmethod enqueue-program
    ((queue pq-program-queue) program size has-hole? trace)
  (priority-queue:pqueue-push
   (%make-program-queue-node program size has-hole? trace)
   size
   (pq-unwrap queue)))

(defmethod dequeue-program ((queue pq-program-queue))
  (let ((entry (priority-queue:pqueue-pop (pq-unwrap queue))))
    (values (program-queue-node-program entry)
            (program-queue-node-size entry)
            (program-queue-node-has-hole? entry)
            (program-queue-node-trace entry))))

(defmethod depth ((queue pq-program-queue))
  (priority-queue:pqueue-length (pq-unwrap queue)))

;;;
;;; dang fast queue implementations
;;;
(defclass dfq-wrapper ()
  ((queue :initarg :queue :initform (damn-fast-priority-queue:make-queue))))

(defclass dfsq-wrapper ()
  ((queue :initarg :queue :initform (damn-fast-stable-priority-queue:make-queue))))

(defmethod make-queue ((descriptor (eql :df)) initial-size)
  (let ((args (when initial-size (list initial-size))))
    (make-instance 'dfq-wrapper
                   :queue (apply #'damn-fast-priority-queue:make-queue args))))

(defmethod make-queue ((descriptor (eql :dfs)) initial-size)
  (let ((args (when initial-size (list initial-size))))
    (make-instance 'dfsq-wrapper
                   :queue (apply #'damn-fast-stable-priority-queue:make-queue args))))

(declaim (inline dfq-unwrap))
(defun dfq-unwrap (dfq-wrapper)
  "Unwraps a dang fast queue"
  (declare (type dfq-wrapper dfq-wrapper))
  (slot-value dfq-wrapper 'queue))

(declaim (inline dfsq-unwrap))
(defun dfsq-unwrap (dfsq-wrapper)
  "Unwraps a dang fast queue"
  (declare (type dfsq-wrapper dfsq-wrapper))
  (slot-value dfsq-wrapper 'queue))

(defmethod enqueue-program ((queue dfq-wrapper) program size has-hole? trace)
  (damn-fast-priority-queue:enqueue
   (dfq-unwrap queue)
   (%make-program-queue-node program size has-hole? trace)
   size))

(defmethod enqueue-program ((queue dfsq-wrapper) program size has-hole? trace)
  (damn-fast-stable-priority-queue:enqueue
   (dfsq-unwrap queue)
   (%make-program-queue-node program size has-hole? trace)
   size))

(defmethod dequeue-program ((queue dfq-wrapper))
  (let ((entry (damn-fast-priority-queue:dequeue (dfq-unwrap queue))))
    (values (program-queue-node-program entry)
            (program-queue-node-size entry)
            (program-queue-node-has-hole? entry)
            (program-queue-node-trace entry))))

(defmethod dequeue-program ((queue dfsq-wrapper))
  (let ((entry (damn-fast-stable-priority-queue:dequeue (dfsq-unwrap queue))))
    (values (program-queue-node-program entry)
            (program-queue-node-size entry)
            (program-queue-node-has-hole? entry)
            (program-queue-node-trace entry))))

(defmethod depth ((queue dfq-wrapper))
  (damn-fast-priority-queue:size (dfq-unwrap queue)))

(defmethod depth ((queue dfsq-wrapper))
  (damn-fast-stable-priority-queue:size (dfsq-unwrap queue)))
;;;
;;; Custom queue implementation
;;;
(defclass program-queue ()
  ((head-vector :accessor %head-vector
                :initarg :head-vector
                :documentation "Vector of queue heads by size")
   (end-vector :accessor %end-vector
               :initarg :end-vector
               :documentation "Vector of queue ends by size")
   (current-priority :accessor current-priority
                     :initarg :current-priority
                     :documentation "Points to the current priority on the queue")
   (depth :accessor depth
          :initarg :depth
          :documentation "Queue depth of this queue")
   (scale-factor :reader %scale-factor
                 :initarg :scale-factor
                 :type number
                 :documentation "Amount to scale the queue vectors by. >1.")
   (free-entries :accessor %free-entries
                 :initarg :free-entries
                 :documentation "Queue entries that can be reused"))
  (:default-initargs :head-vector (make-array 10
                                              :initial-element nil
                                              :adjustable t)
                     :end-vector (make-array 10
                                             :initial-element nil
                                             :adjustable t)
                     :current-priority 0
                     :depth 0
                     :scale-factor 1.7
                     :free-entries nil)
  (:documentation "A priority queue designed for accepting integer priorities, starting
at zero and increasing. Once entries of a given priority are finished dequeuing, no
entries of that priority or lower may be enqueued."))

(defmethod make-queue ((designator (eql :new)) initial-size)
  (declare (ignore initial-size))
  (make-instance 'program-queue))

(defun %reinit-program-queue-node (node program size has-hole? trace)
  "Re-initializes a program queue node"
  (setf (program-queue-node-program node) program
        (program-queue-node-size node) size
        (program-queue-node-has-hole? node) has-hole?
        (program-queue-node-next node) nil
        (program-queue-node-trace node) trace)
  node)

(defun %deinit-program-queue-node (node)
  "De-initializes a program queue node"
  (setf (program-queue-node-program node) nil
        (program-queue-node-size node) nil
        (program-queue-node-has-hole? node) nil
        (program-queue-node-trace node) nil
        (program-queue-node-next node) nil)
  node)

(defun %ensure-program-queue-room (queue size)
  "Ensures that the underlying data structure of QUEUE can handle a priority SIZE."
  (declare (type program-queue queue)
           (type (integer 0 *) size))
  (with-accessors ((h-vec %head-vector)
                   (e-vec %end-vector)
                   (scale %scale-factor))
      queue
    (when (<= (length h-vec) size)
      (let ((new-size (max (1+ size)
                           (ceiling (* (length h-vec) scale)))))
        (format t "QUEUE ADJUST [~a --> ~a]~%" (length h-vec) new-size)
        (setf h-vec (adjust-array h-vec new-size :initial-element nil))
        (setf e-vec (adjust-array e-vec new-size :initial-element nil))))
    nil))

;;;
;;; Here are a couple of utility macros for pushing/popping whole cons cells from lists
;;;
(defun %push-node (list node)
  "Pushes NODE destructively onto the head of LIST"
  (setf (program-queue-node-next node) list))

(define-modify-macro %pushf-node (node) %push-node)

(defmacro %popf-node (list &environment env)
  "Pops a node from LIST destructively"
  (let ((ret-v (gensym)))
    (multiple-value-bind (dummies vals new setter getter)
        (get-setf-expansion list env)
      `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
         (if ,(cdr new) (error "Can't expand this."))
         (let ((,ret-v ,(car new)))
           (unless (null ,ret-v)
             (setq ,(car new) (program-queue-node-next ,(car new)))
             ,setter
             (setf (program-queue-node-next ,ret-v) nil))
           ,ret-v)))))

(declaim (inline %make-queue-entry))
(defun %make-queue-entry (queue program size has-hole? trace)
  "Makes a queue entry"
  (*:if-let (entry (%popf-node (%free-entries queue)))
    (%reinit-program-queue-node entry program size has-hole? trace)
    (%make-program-queue-node program size has-hole? trace)))

(defmethod enqueue-program
    ((queue program-queue) program size has-hole? trace)
  "Enqueues PROGRAM having size SIZE into QUEUE."
  (declare (type integer size)
           (type program-queue queue))
  (assert (>= size (current-priority queue)))
  (%ensure-program-queue-room queue size)
  (symbol-macrolet ((head (aref (%head-vector queue) size))
                    (end (aref (%end-vector queue) size)))
    (flet ((enqueue-empty (entry)
             (assert (null head))
             (assert (null end))
             (setf head entry)
             (setf end entry))
           (enqueue-normal (entry)
             (assert (*:true head))
             (assert (*:true end))
             (setf (program-queue-node-next end) entry)
             (setf end entry)))
      (let ((new-node (%make-queue-entry queue program size has-hole? trace)))
        (if (null end)
            (enqueue-empty new-node)
            (enqueue-normal new-node))))
    (incf (depth queue))
    nil))

(defun %ensure-current-pointer (queue)
  "Checks, and possibly advances, the current pointer"
  (assert (not (zerop (depth queue))))
  (loop while (null (aref (%head-vector queue) (current-priority queue)))
        do (incf (current-priority queue)))
  (current-priority queue))

(defmethod dequeue-program ((queue program-queue))
  "Dequeues a program from QUEUE"
  (declare (type program-queue queue))
  (let ((curr (%ensure-current-pointer queue)))
    (symbol-macrolet ((head (aref (%head-vector queue) curr))
                      (end (aref (%end-vector queue) curr)))
      (let* ((entry (%popf-node head)))
        (multiple-value-prog1 (values (program-queue-node-program entry)
                                      (program-queue-node-size entry)
                                      (program-queue-node-has-hole? entry)
                                      (program-queue-node-trace entry))
          (when (null head) (setf end nil))
          (%pushf-node (%free-entries queue) (%deinit-program-queue-node entry))
          (decf (depth queue)))))))
