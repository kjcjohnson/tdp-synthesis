(in-package #:com.kjcjohnson.tdp)
(kl/oo:import-classes-from #:vsa)

(defclass enumerator-algorithm () ())

(defclass enumerator-info ()
  ((depth :initarg :max-depth)
   (inputs :initarg :inputs :reader inputs)
   (outputs :initarg :outputs :reader outputs)))

(defmethod infer ((prod g:production) child-ix (info enumerator-info) context)
  (declare (ignore context))
  (let ((current-depth (slot-value info 'depth)))
    (if (> current-depth 1)
        (make-instance 'enumerator-info :max-depth (1- (slot-value info 'depth)))
        nil)))

(defmethod synthesize-dispatch ((algorithm enumerator-algorithm) nt-or-prod info)
  (cond ((null info)
         nil)
        ((string= (g:name nt-or-prod) "Constant")
         nil) ; Cannot enumerate constants
        ((and (slot-boundp info 'inputs)
              (slot-boundp info 'outputs))
         :check-wrapper)
        (t
         :tdp)))

(defmethod synthesize* ((obj (eql :check-wrapper)) nt-or-prod (info enumerator-info))
  (let* ((new-info (make-instance 'enumerator-info :max-depth (slot-value info 'depth)))
         (programs (synthesize nt-or-prod new-info)))

    (let (min-prog
          (next-checkpoint (+ 10 (get-universal-time)))
          (p-count 0)
          (a-count 0)
          (t-count (program-node:program-count programs)))
      (format t "~&Number of programs: ~a~%" t-count)
      (kl:foreach (candidate in programs)
        (when (and
               (or (null min-prog)
                   (< (ast:program-size candidate)
                      (ast:program-size min-prog)))
               (every #'(lambda (exs)
                          (equal (ast:execute-program *semantics*
                                                      candidate
                                                      (car exs))
                                 (cadr exs)))
                      (mapcar #'list (inputs info) (outputs info))))
          (setf min-prog candidate)
          (format t "~&Good [~s]: ~a~%" (ast:program-size candidate) candidate))
        (incf p-count)
        (incf a-count)
        (when (< next-checkpoint (get-universal-time))
          (format t "~&Rate: ~a programs/s [~a%]~%"
                  (float (/ p-count 10))
                  (float (/ a-count t-count)))
          (setf next-checkpoint (+ 10 (get-universal-time))
                p-count 0)))
    min-prog)))

(defmethod combine ((prod g:production) children ci)
  (if (zerop (g:arity prod))
      (leaf-program-node:new (make-instance 'ast:program-node :production prod))
      (cross-program-node:new prod children)))
