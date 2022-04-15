;;;;
;;;; Package definitions
;;;;
(defpackage #:com.kjcjohnson.tdp
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo))
  (:export #:synthesize
           #:synthesize-dispatch
           #:synthesize*
           #:combine
           #:infer
           #:derive
           #:*algorithm*
           #:*semantics*
           #:*grammar*))

(defpackage #:com.kjcjohnson.tdp.test
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:semgus-i/o #:com.kjcjohnson.synthkit.semgus.interop)
                    (#:tdp #:com.kjcjohnson.tdp)
                    (#:ast #:com.kjcjohnson.synthkit.ast)))
