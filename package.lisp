;;;;
;;;; Package definitions
;;;;
(defpackage #:com.kjcjohnson.tdp
  (:use #:cl)
  (:local-nicknames (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:vsa #:com.kjcjohnson.synthkit.vsa)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:a #:alexandria)
                    (#:kl #:com.kjcjohnson.kale)
                    (#:kl/oo #:com.kjcjohnson.kale.oo))
  (:export #:synthesize
           #:synthesize-dispatch
           #:synthesize*
           #:combine

           ;; Propagation
           #:infer
           #:derive

           ;; Search Strategies
           #:get-search-strategy
           #:is-search-done
           #:get-next-search-query
           #:search-transition
           #:get-search-result

           ;; SIPS
           #:sideways-information-passing-strategy
           #:sips.ltr
           #:sips.final
           #:sips.nth-child
           #:sips.nth-dependencies
           #:get-sips

           ;; Contexts
           #:context
           #:context.up
           #:context.down
           #:context.information-equivalent?
           #:context.merge-information
           #:context.merge

           ;; Miscellaneous
           #:ensure-list
           #:all-cart-prod
           #:define-init-hook
           #:*algorithm*
           #:*semantics*
           #:*grammar*
           #:*semgus-problem*
           #:*depth*))

