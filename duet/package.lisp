;;;;
;;;; Package definition for Duet-like solver
;;;;

(defpackage #:com.kjcjohnson.tdp.duet
  (:use #:cl)

  (:local-nicknames (#:tdp #:com.kjcjohnson.tdp)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:enum #:com.kjcjohnson.tdp.enumerative)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)
                    (#:kl/c #:com.kjcjohnson.kale.collections))

  (:export #:component-library-generate
           #:component-library-task
           #:duet-solve))
