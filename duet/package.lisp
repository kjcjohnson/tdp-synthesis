;;;;
;;;; Package definition for Duet-like solver
;;;;

(defpackage #:com.kjcjohnson.tdp.duet
  (:use #:cl)

  (:local-nicknames (#:tdp #:com.kjcjohnson.tdp)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:enum #:com.kjcjohnson.tdp.enumerative)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)
                    (#:kl/c #:com.kjcjohnson.kale.collections))

  (:export #:component-library-generate
           #:component-library-task))