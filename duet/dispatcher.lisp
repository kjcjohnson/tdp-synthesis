;;;;
;;;; The Duet dispatcher
;;;;
(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:kl/c)

(defclass cache-entry ()
  ((programs :initarg :programs :reader cache-entry%programs)
   (qualifier :initarg :qualifier :reader cache-entry%qualifier))
  (:documentation "Entry in program cache with qualifier"))

(let ((task-cache (dictionary:new)))

  (tdp:define-init-hook add-clear-task-cache
    "Adds hook to clear the task cache after each enumeration"
    (when (typep tdp:*algorithm* 'duet-algorithm)
      (add-reset-hook tdp:*algorithm*
                      'clear-task-cache
                      (lambda ()
                        (setf task-cache (dictionary:new))))))
  
  (defun %cache-for-g-elt (nt-or-prod)
    "Gets the cache for the particular grammar element"
    (multiple-value-bind (cache present?) (&dictionary:get task-cache nt-or-prod)
      (unless present?
        (setf cache (dictionary:new :test #'kl:equals))
        (&dictionary:add task-cache nt-or-prod cache))
      cache))
  
  (defun is-task-cached? (nt-or-prod info)
    "Checks if the current requested task is already in-progress or cached"
    (multiple-value-bind (val present)
        (&dictionary:get (%cache-for-g-elt nt-or-prod) info)
      (and present (<= (cache-entry%qualifier val) tdp:*depth*))))

  (defun is-task-in-progress? (nt-or-prod info)
    "Checks if the current requested task is in-progress"
    (multiple-value-bind (val present)
        (&dictionary:get (%cache-for-g-elt nt-or-prod) info)
      (and present (null val))))
  
  (defun add-in-progress-task (nt-or-prod info)
    "Marks a task as being in-progress"
    (&dictionary:add (%cache-for-g-elt nt-or-prod) info nil))

  (defun set-task-completed (nt-or-prod info result)
    "Updates a task as being complete"
    (&dictionary:add (%cache-for-g-elt nt-or-prod)
                     info
                     (make-instance 'cache-entry
                                    :programs result
                                    :qualifier tdp:*depth*)))

  (defun get-task-result (nt-or-prod info)
    "Gets information from a completed task"
    (cache-entry%programs (&dictionary:get (%cache-for-g-elt nt-or-prod) info))))

(defmethod tdp:synthesize* :around ((specializer (eql :tdp))
                                    nt-or-prod
                                    (info duet-information))
  "Handles checking and updating caches"
  '(format *trace-output* "~&~a~a ~a?~%"
          (make-string tdp:*depth* :initial-element #\Space)
          nt-or-prod
          (duet-information:outputs info))
  (if (is-task-cached? nt-or-prod info)
      (progn
        '(format *trace-output* "~&~aCACHE-HIT: ~a ~a~%"
                (make-string tdp:*depth* :initial-element #\Space)
                nt-or-prod
                (duet-information:outputs info))
        (get-task-result nt-or-prod info))
      (let (result)
        (when (is-task-in-progress? nt-or-prod info) (error "Bad"))
        (add-in-progress-task nt-or-prod info)
        (setf result (call-next-method))
        (set-task-completed nt-or-prod info result)
        '(format *trace-output* "~&~aCACHE-ADD: ~a ~a~%"
                (make-string tdp:*depth* :initial-element #\Space)
                nt-or-prod
                (duet-information:outputs info))
        '(kl:foreach (program in result)
          (format *trace-output* "~&~a      ~a~%"
                  (make-string tdp:*depth* :initial-element #\Space)
                  program))
        result)))

(defmethod tdp:synthesize-dispatch ((algo duet-algorithm) nt-or-prod (info duet-information))
  "Dispatcher for Duet synthesis problems."
  (cond
    ((null info)
     nil)

    ((> tdp:*depth* 30)
     nil)

    ((not (null (duet-information:refinement info)))
     'check-refinement)

    ((null (duet-information:outputs info))
     'component-library-task)
    
    ((is-task-in-progress? nt-or-prod info)
     '(format *trace-output* "~&;; Loop: ~s [~s]~%" (duet-information:outputs info) nt-or-prod)
     nil) ;; Looping
    
    ((or (not (boundp '*component-library*))
         (null *component-library*))
     'component-library-generate)

    ((and (typep nt-or-prod 'g:non-terminal)
          (slot-value info 'check-library?))
     'check-component-library)
    
    (t
     :tdp)))

(defmethod tdp:synthesize-dispatch ((algo duet-algorithm)
                                    nt-or-prod
                                    (info disjunctive-duet-information))
  'split-disjunctive)

(defmethod tdp:synthesize-dispatch ((algorithm duet-algorithm) nt-or-prod info)
  (cond
    ((null info)
     nil)
    ((eql info :top)
     'component-library-task)
    (t
     (error "Unknown info: ~a" info))))

(defmethod tdp:synthesize-dispatch ((algorithm duet-algorithm) nt-or-prod (info enum::enumerator-info))
  
  (cond ((null info)
         nil)
        ((string= (g:name nt-or-prod) "Constant")
         nil) ; Cannot enumerate constants
        ((and (slot-boundp info 'enum::inputs)
              (slot-boundp info 'enum::outputs))
         'enumerative-filter-task)
        (t
         :tdp)))


(defmethod tdp:synthesize* ((obj (eql 'split-disjunctive))
                            nt-or-prod
                            (info disjunctive-duet-information))
  (make-instance 'vsa:union-program-node
                 :programs
                 (map 'list #'(lambda (i) (tdp:synthesize nt-or-prod i))
                      (disjunctive-duet-information:sub-specifications info))))

(defmethod tdp:synthesize* ((obj (eql 'check-refinement))
                            nt-or-prod
                            (info duet-information))
  "Checks and filters a refinement condition."
  (let ((new-info (duet-information:copy info)))
    (setf (duet-information:refinement new-info) nil)
    (let ((candidates (tdp:synthesize nt-or-prod new-info)))
      (vsa:filter candidates
                  (duet-information:inputs info)
                  (duet-information:refinement info)
                  tdp:*semantics*
                  :test #'(lambda (actual refinement)
                            (funcall (refinement-function refinement)
                                     (smt:get-first-value actual)))))))
