;;;;
;;;; Globals used in the tdp framework
;;;;
(in-package #:com.kjcjohnson.tdp)

(defvar *algorithm* nil "The algorithm object for the current synthesis execution")
(defvar *grammar* nil "The grammar for the current synthesis execution")
(defvar *semantics* nil "The semantics for the current synthesis execution")

(defun tdp (algorithm grammar semantics info &key (print t))
  (let ((*algorithm* algorithm)
        (*grammar* grammar)
        (*semantics* semantics))
    (let ((result (synthesize (g:initial-non-terminal *grammar*) info)))
      (when print
        (kl:foreach (p in result)
          (format t "~a~%" p))))))
