;;;;
;;;; ks2 integration package
;;;;
(defpackage #:com.kjcjohnson.tdp.ks2.solver-api
  (:use #:cl #:com.kjcjohnson.ks2.solver-api)
  (:local-nicknames (#:s-api #:com.kjcjohnson.ks2.solver-api)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:enum #:com.kjcjohnson.tdp.enumerative)
                    (#:duet #:com.kjcjohnson.tdp.duet)
                    (#:frangel #:com.kjcjohnson.frangel)
                    (#:tde #:com.kjcjohnson.tdp.top-down-enum)))
