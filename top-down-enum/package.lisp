;;;;
;;;; Top-down enumerator - package definition
;;;;
(defpackage #:com.kjcjohnson.tdp.top-down-enum
  (:use #:cl)

  (:local-nicknames (#:tdp #:com.kjcjohnson.tdp)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:u #:com.kjcjohnson.synthkit.utilities)
                    (#:* #:serapeum/bundle)
                    (#:? #:trivia)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo)
                    (#:kl/c #:com.kjcjohnson.kale.collections))

  (:export #:top-down-enum-solve))
