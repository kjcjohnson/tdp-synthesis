;;;;
;;;; New filling
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defun enqueue-pe (queue entry)
  "Enqueues a program entry"
  (damn-fast-stable-priority-queue:enqueue
   queue
   entry
   (pe-size entry)))

(defun dequeue-pe (queue)
  "Dequeues a program entry"
  (damn-fast-stable-priority-queue:dequeue
   queue))

(defun depth-pe (queue)
  "Gets the queue depth"
  (damn-fast-stable-priority-queue:size queue))

(defmethod tdp:synthesize* ((obj (eql 'top-down-new))
                            nt
                            (info initial-information))
  (format t "~&-----~%Defer Prune: ~a~%Remove Pruned: ~a~%-----~%"
          *defer-prune* *remove-pruned*)
  (when *collect-prune-stats* (reset-prune-stats))
  (let ((pq (damn-fast-stable-priority-queue:make-queue))
        (timer (get-internal-real-time))
        (prune-strategy (setup-prune-strategy))
        (fragments nil))

    (multiple-value-bind (initial-pe frags)
        (get-initial-entry tdp:*grammar*)
      ;; Enqueue the initial entry - just a single hole
      (enqueue-pe pq initial-pe)
      (setf fragments frags))

    (loop
      (let ((candidate-pe (dequeue-pe pq))
            (trace nil))
        (assert (pe-has-hole? candidate-pe))

        (when (> (get-internal-real-time) timer)
          (incf timer (* internal-time-units-per-second 5))
          (format *trace-output* "~&; PQ Depth: ~a~%" (depth-pe pq)))

        (unless (and *defer-prune*
                     (try-prune-candidate (pe-program candidate-pe)
                                          trace prune-strategy)
                     *remove-pruned*)
          (let ((next (u:with-timed-section (*enumerate-section*)
                        (expand-next-hole candidate-pe fragments))))

            ;;(assert (has-change? next))
            (dolist (pe next)
              (let ((size (pe-size pe))
                    (program (pe-program pe)))
                ;;(format *trace-output* "~&[~a] ~a~%" size program)
                (if (pe-has-hole? pe)
                    (progn
                      (incf ast:*candidate-partial-programs*)
                      (if (and (not *defer-prune*)
                               (try-prune-candidate program
                                                    nil ;;(hole-trace pr)
                                                    prune-strategy)
                               *remove-pruned*)
                          nil
                          (enqueue-pe pq pe)))
                    (progn
                      (incf ast:*candidate-concrete-programs*)
                      (when (= 1 (incf (getf ast:*concrete-candidates-by-size* size 0)))
                        (ast:add-checkpoint size))
                      (ast:trace-program program)
                      (when (semgus:check-program tdp:*semgus-problem* program)
                        (format t "; FOUND: [~a] ~a~%" size program)
                        (when *collect-prune-stats* (report-prune-stats))
                        (format t "~&; ENUM: TIME: ~,2fs; GC: ~,2fs; ALLOC: ~,3f MiB~%"
                                (u:get-timed-section-real-time *enumerate-section*)
                                (u:get-timed-section-gc-time *enumerate-section*)
                                (u:get-timed-section-bytes-consed *enumerate-section*
                                                                  :unit "MiB"))
                        (return-from tdp:synthesize*
                          (make-instance 'vsa:leaf-program-node
                                         :program program)))))))))))))
