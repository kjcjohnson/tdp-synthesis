(in-package #:com.kjcjohnson.tdp)

;;;
;;; Contextual information - downward and upward information
;;;
(kl/oo:define-encapsulated-class information-context

  (public constructor (root up down)
          (setf root-info root
                _upward-info up
                _downward-info down))
    
  (private field _upward-info)
  (private field _downward-info)
    
  (public property root-info)

  (public upward-info (index)
          (nth index _upward-info))

  (public downward-info (index)
          (nth index _downward-info))

  (public copy ()
          (information-context:new root-info
                                   (copy-list _upward-info)
                                   (copy-list _downward-info))))
