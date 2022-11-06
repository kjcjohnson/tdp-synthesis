;;;;
;;;; Enumerative solver
;;;;
(defpackage #:com.kjcjohnson.tdp.enumerative
  (:use #:cl)

  (:local-nicknames (#:tdp #:com.kjcjohnson.tdp)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)
                    (#:kl/c #:com.kjcjohnson.kale.collections))

  (:export #:enumerative-filter-task
           #:enumerator-info))
