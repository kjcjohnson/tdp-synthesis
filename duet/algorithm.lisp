;;;;
;;;; Duet algorithm details
;;;;
(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:kl/c)

(defclass duet-algorithm ()
  ((reset-hooks :initarg :reset-hooks :accessor reset-hooks :documentation "Hooks to run when starting a new propagation run")
   (inverse-semantics :initarg :inverse-semantics
                      :accessor duet%inverse-semantics))
  (:default-initargs :reset-hooks nil
                     :inverse-semantics (dictionary:new)))

(defun add-reset-hook (algo name hook)
  "Adds a reset hook"
  (let ((res (assoc name (reset-hooks algo))))
    (if (null res)
        (setf (reset-hooks algo) (acons name hook (reset-hooks algo)))
        (setf (cdr res) hook))))

(defun run-reset-hooks (algo)
  "Runs the reset hooks"
  (loop for (name . hook) in (reset-hooks algo)
        doing (funcall hook)))

(defun any-inverse-semantics? (name)
  "Checks if an operator NAME has any associated inverse semantics"
  (nth-value 1 (&dictionary:get (duet%inverse-semantics tdp:*algorithm*)
                                (smt:ensure-identifier name))))

(defun get-inverse-semantics (name child-ix)
  "Gets the inverse semantic function for an operator, or null if not defined."
  (let* ((semlist (&dictionary:get (duet%inverse-semantics tdp:*algorithm*)
                                   (smt:ensure-identifier name)))
         (cell (assoc child-ix semlist)))
    (cdr cell)))

(defun add-inverse-semantics (name child-ix fn)
  "Adds an inverse for the given operator and child.
FN is a function of two arguments: the target value and the context."
  (let* ((semlist (&dictionary:get (duet%inverse-semantics tdp:*algorithm*)
                                   (smt:ensure-identifier name)))
         (cell (assoc child-ix semlist)))
    (if (null cell)
        (setf semlist (acons child-ix fn semlist))
        (setf (cdr cell) fn))
    (&dictionary:add (duet%inverse-semantics tdp:*algorithm*)
                     (smt:ensure-identifier name)
                     semlist)))
    
(defmacro definv (name child-ix (inputs-var output-var context-var &rest keys) &body body)
  `(add-inverse-semantics ,name
                          ,child-ix
                          (lambda (,inputs-var
                                   ,output-var
                                   ,context-var
                                   ,@keys
                                   ,@(unless (find '&key keys) '(&key))
                                   &allow-other-keys)
                            (declare (ignorable ,inputs-var
                                                ,output-var
                                                ,context-var))
                            ,@body)))
