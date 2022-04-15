(in-package #:com.kjcjohnson.tdp.test)

;;;
;;; Left-to-right search strategy with dependency tracking
;;
(defclass ltr-dep-search-strategy ()
  ((info :initarg :info)))

(defclass ltr-dep-search-state ()
  ((

;; (defclass ltr-dep-context ()
;;   ((

(defmethod get-search-strategy (algorithm nt-or-prod info)
  (values (make-instance 'ltr-dep-search-strategy :info info) (cons nil 0)))
  
(defmethod is-search-done ((prod g:production) (strategy ltr-dep-search-strategy) state)
  (>= (cdr state) (length (g:occurrences prod))))

(defmethod get-next-search-query ((prod g:production) (strategy ltr-dep-search-strategy) state)
  (values (nth (cdr state) (g:occurrences prod))
          (cdr state)
          (slot-value strategy 'info)))

(defmethod search-transition ((prod g:production)
                              (strategy ltr-dep-search-strategy)
                              state
                              prog-set
                              up-info)
  (cons (append (car state) (list prog-set)) (1+ (cdr state))))

(defmethod get-search-result ((prod g:production) (strategy ltr-dep-search-strategy) state)
  (combine prod (car state) (slot-value strategy 'info)))
