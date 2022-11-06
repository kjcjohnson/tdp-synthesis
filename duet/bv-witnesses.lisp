;;;;
;;;; Bit vector witness functions
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

(tdp:define-init-hook bv-witnesses
  (when (typep tdp:*algorithm* 'duet-algorithm)

    ;(definv "bvadd" 1 (in out ctx)

    (definv "bv
