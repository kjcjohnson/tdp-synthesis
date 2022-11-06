;;;;
;;;; Testing package
;;;;

(defpackage #:com.kjcjohnson.tdp.test
  (:use #:cl)
  (:local-nicknames (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:tdp #:com.kjcjohnson.tdp)
                    (#:enum #:com.kjcjohnson.tdp.enumerative)
                    (#:duet #:com.kjcjohnson.tdp.duet)
                    (#:frangel #:com.kjcjohnson.frangel)
                    (#:tde #:com.kjcjohnson.tdp.top-down-enum)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:smt #:com.kjcjohnson.synthkit.smt)))
