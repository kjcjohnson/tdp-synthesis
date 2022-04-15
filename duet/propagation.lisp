(in-package #:com.kjcjohnson.tdp.duet)

(defclass universal-context ()
  ((previous-sets :initarg :previous)))

;;;
;;; Universal search
;;;
(defclass universal-search-strategy ()
  ((info :initarg :info)))

(defmethod get-search-strategy ((algorithm duet-algorithm)
                                nt-or-prod
                                (info duet-information))
  (values (make-instance 'left-to-right-search-strategy :info info) (cons nil 0)))
  
(defmethod is-search-done ((prod g:production) (strategy left-to-right-search-strategy) state)
  (>= (cdr state) (length (g:occurrences prod))))

(defmethod get-next-search-query ((prod g:production) (strategy left-to-right-search-strategy) state)
  (values (nth (cdr state) (g:occurrences prod))
          (cdr state)
          (slot-value strategy 'info)))

(defmethod search-transition ((prod g:production)
                              (strategy left-to-right-search-strategy)
                              state
                              prog-set
                              up-info)
  (cons (append (car state) (list prog-set)) (1+ (cdr state))))

(defmethod get-search-result ((prod g:production) (strategy left-to-right-search-strategy) state)
  (combine prod (car state) (slot-value strategy 'info)))


(defun universal-witness (prod child-ix ctx)
  (if (= (g:arity prod) (1+ child-ix))
      (progn
        (
        )
      :top)

(defmethod tdp:infer ((prod g:production) child-ix (info duet-information))
  nil)


