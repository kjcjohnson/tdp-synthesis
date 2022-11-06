(in-package #:com.kjcjohnson.tdp.enumerative)


;;;
;;; Default left-to-right search
;;;
(defclass enum-search-strategy ()
  ((info :initarg :info)))

(defmethod tdp:get-search-strategy (algorithm
                                    (prod g:production)
                                    (info enumerator-info))
  (values (make-instance 'enum-search-strategy :info info) (cons nil 0)))
  
(defmethod tdp:is-search-done ((prod g:production)
                               (strategy enum-search-strategy)
                               state)
  (>= (cdr state) (length (g:occurrences prod))))

(defmethod tdp:get-next-search-query ((prod g:production)
                                      (strategy enum-search-strategy)
                                      state)
  (values (nth (cdr state) (g:occurrences prod))
          (cdr state)
          (slot-value strategy 'info)))

(defmethod tdp:search-transition ((prod g:production)
                                  (strategy enum-search-strategy)
                                  state
                                  prog-set
                                  up-info)
  (cons (append (car state) (list prog-set)) (1+ (cdr state))))

(defmethod tdp:get-search-result ((prod g:production)
                              (strategy enum-search-strategy)
                              state)
  (tdp:combine prod (car state) (slot-value strategy 'info)))
          
