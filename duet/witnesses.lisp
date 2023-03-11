;;;;
;;;; witnesses.lisp - some infrastructure for mapping productions to witness names
;;;;
(in-package :com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:kl/c)

(let (witness-name-cache)
  (tdp:define-init-hook clear-witness-cache
    (setf witness-name-cache (dictionary:new)))
  
  (defun map-production-to-witness-name (production
                                         &optional (semgus-problem
                                                    tdp:*semgus-problem*))
    (multiple-value-bind (name cached)
        (&dictionary:get witness-name-cache production)
      (unless cached
        (setf name (map-production-to-witness-name* production semgus-problem))
        (&dictionary:add witness-name-cache
                         production
                         name))
      name)))
  

(defun map-production-to-witness-name* (production semgus-problem)
  "Maps a production to a canonical name for witnessing."
  (let* ((ctx (semgus:context semgus-problem))
         (chcs (semgus:chcs ctx)))
    ;;
    ;; First, find all applicable productions
    ;;
    (setf chcs (remove-if-not #'(lambda (chc)
                                  (eql (g:name (g:operator production))
                                       (chc:name (chc:constructor chc))))
                              chcs))

    ;;
    ;; Analyze the CHC constraints and see what we have
    ;;
    (let ((names (map 'list
                      #'(lambda (chc)
                          (let ((constraint (chc:constraint chc))
                                (name nil))
                            (cond
                              ;;
                              ;; Standard. Just equals a function call
                              ;;
                              ((smt:is-application? constraint "=")
                               (kl:foreach (child in (smt:children constraint))
                                 (when (smt:is-application? child)
                                   (setf name (smt:name child)))))
                              ;;
                              ;; Just true. Just identity?
                              ;;
                              ((smt:is-application? constraint "true")
                               (setf name (smt:ensure-identifier "identity"))))
                            name))
                      chcs)))
      (when (not (every #'eql names (rest names)))
        (warn "Different witnesses inferred for production: ~a. Got: ~a"
              production names))
      (when (every #'null names)
        (warn "No witness inferred for production: ~a" production))
      (first names))))
        
