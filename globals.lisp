;;;;
;;;; Globals used in the tdp framework
;;;;
(in-package #:com.kjcjohnson.tdp)

(defvar *algorithm* nil "The algorithm object for the current synthesis execution")
(defvar *grammar* nil "The grammar for the current synthesis execution")
(defvar *semantics* nil "The semantics for the current synthesis execution")
(defvar *semgus-problem* nil "The semgus problem for the current synthesis execution")
(defvar *depth* 0 "The current synthesis depth")
(defvar *current-program* nil "The current program being analyzed")

;;;
;;; Memory limiting
;;;
#+sbcl
(progn
  (defparameter *memory-limit-threshold* 0.10
    "The percentage of dynamic space to set for the memory limit.")
  (defparameter *memory-gc-reclaimation-threshold* 0.50
    "The percentage of dynamic space to trigger termination after GC.")
  (defparameter *memory-limit*
    (floor (* *memory-limit-threshold* (sb-ext:dynamic-space-size)))
    "The limit after which to examine a synthesis run for termination.
Before terminating, a full GC pass is made, and if enough space is reclaimed, the
run will continue. Otherwise, an error condition is signalled.")
  (defparameter *memory-gc-reclaimation-limit*
    (floor (* *memory-gc-reclaimation-threshold* (sb-ext:dynamic-space-size)))))

(define-condition memory-limit-exceeded (error)
  ((memory-usage-limit :reader memory-usage-limit :initarg :memory-usage-limit)
   (triggering-usage :reader triggering-usage :initarg :triggering-usage)
   (post-gc-usage-limit :reader post-gc-usage-limit :initarg :post-gc-usage-limit)
   (post-gc-usage :reader post-gc-usage :initarg :post-gc-usage))
  (:report (lambda (condition stream)
             (format stream "Memory limit exceeded.~%
  |--> Pre-gc : ~a of ~a bytes~%  |--> Post-gc: ~a of ~a bytes~%"
                     (triggering-usage condition)
                     (memory-usage-limit condition)
                     (post-gc-usage condition)
                     (post-gc-usage-limit condition)))))   
    

(defmacro with-depth-layer (&body body)
  "Increments the current depth layer."
  `(let ((*depth* (1+ *depth*)))
    ,@body))

(defvar *init-hooks* nil "Functions to run when starting a fresh run of the TDP algorithm")
(defun add-init-hook (name hook)
  "Adds an initialization hook for the TDP algorithm."
  (let ((res (assoc name *init-hooks*)))
    (if (null res)
        (setf *init-hooks* (acons name hook *init-hooks*))
        (setf (cdr res) hook))))
  
(defmacro define-init-hook (name &body body)
  "Defines an initialization hook for the TDP algorithm."
  `(add-init-hook ',name (lambda () ,@body)))

(defmacro with-memory-limit (&body body)
  #+sbcl
  `(let ((old-hooks sb-kernel::*after-gc-hooks*))
     (unwind-protect
          (progn
            (push #'(lambda ()
                      (let ((usage (sb-kernel:dynamic-usage)))
                        (when (> usage *memory-limit*)
                          (error (make-condition 'memory-limit-exceeded
                                                 :triggering-usage usage
                                                 :memory-usage-limit *memory-limit*)))))
                  sb-kernel::*after-gc-hooks*)
            ,@body)
       (setf sb-kernel::*after-gc-hooks* old-hooks)))
  #-sbcl
  `(progn
     (warn "Memory limits not supported on this platform - only on SBCL.")
     ,@body))

(let (last-program-print-time)

  (define-init-hook clear-last-program-print-time
    (setf last-program-print-time nil))

  (defun maybe-print-program ()
    (let ((current-time (get-universal-time)))
      (when (or (null last-program-print-time)
                 (< (+ last-program-print-time)
                    current-time))
        (format *trace-output*
                "; Current program trace: ~a~%"
                (nreverse
                 (map 'list
                      #'(lambda (p)
                          (smt:identifier-string (g:name p)))
                      *current-program*)))
        (setf last-program-print-time current-time)))))


(defmacro with-next-production ((production) &body body)
  "Binds the production under consideration to the current program"
  `(let ((*current-program* (cons ,production *current-program*)))
     (maybe-print-program)
     ,@body))

(defun tdp (algorithm grammar semantics info &key (print t))
  (let ((*algorithm* algorithm)
        (*grammar* grammar)
        (*semantics* semantics))
    (map nil #'(lambda (hook) (funcall (cdr hook))) *init-hooks*)

    (let ((result (synthesize (g:initial-non-terminal *grammar*) info))
          (result-list nil))
      
      (vsa:do-programs (p result)
        (push p result-list)
        (when print
          (format t "~a~%" p)))
      (nreverse result-list))))

(defun tdp-s (semgus-problem algorithm info &key (print t))
  (let ((*semgus-problem* semgus-problem))
    (tdp algorithm
         (semgus:grammar semgus-problem)
         (semgus:semantics semgus-problem)
         info
         :print print)))
