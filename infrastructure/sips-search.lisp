;;;;
;;;; SIPS search - search strategy based on a sidways information passing strategy
;;;;
(in-package #:com.kjcjohnson.tdp)

(defclass sips-search ()
  ((sips :reader sips%sips :initarg :sips)
   (info :reader sips%info :initarg :info))
  (:documentation "Search strategy based on a SIPS"))

(defclass sips-result ()
  ((context :accessor sips%context :initarg :context)
   (program-set :accessor sips%program-set :initarg :program-set))
  (:documentation "Holds a single synthesis result for a child."))

(defclass sips-state ()
  ((index
    :accessor sips%index
    :initform 0
    :documentation "SIPS index currently being searched")
   (context-stack
    :accessor sips%context-stack
    :initform nil
    :documentation "Stack of contexts to be queried")
   (results
    :accessor sips%results)))

(defmethod initialize-instance :after ((state sips-state)
                                       &key production
                                       &allow-other-keys)
  (setf (sips%results state) (make-list (g:arity production))))

(defun sips%get-current-context (state)
  "Gets the current context in the search"
  (if (null (sips%context-stack state))
      (error "Null sips search state context stack")
      (first (sips%context-stack state))))

(defun sips%pop-current-context (state)
  "Pops the current context in the search. Returns T if more to search."
  (pop (sips%context-stack state))
  (not (endp (sips%context-stack state))))

(defun sips%populate-context-stack (search state)
  "Populates the context stack for the next child"
  (unless (null (sips%context-stack state))
    (error "Attempt to populate non-empty context stack"))
  
  (let* ((sips (sips%sips search))
         (ix (sips%index state))
         ;;(cix (sips.nth-child ix sips))
         (deps (sips.nth-dependencies ix sips))
         (contexts (list (make-instance 'context
                                        :arity (arity (sips%sips search))))))
    (kl:foreach (dep in deps)
      (let ((new-contexts))
        (kl:foreach (ctx in contexts)
          (kl:foreach (res in (nth dep (sips%results state)))
            (let ((merged (context.merge ctx (sips%context res))))
              (unless (null merged)
                (push merged new-contexts)))))
        (setf contexts new-contexts)))
    (setf (sips%context-stack state) contexts)))

;;;
;;; Search strategy protocol
;;;
(defmethod get-search-strategy (algorithm
                                (prod g:production)
                                info)
  ;; Return: strategy and initial state
  (let ((search (make-instance 'sips-search
                               :sips (sips.ltr (g:arity prod))
                               :info info))
        (state (make-instance 'sips-state :production prod)))
    (sips%populate-context-stack search state)
    (values search state)))

(defmethod is-search-done ((prod g:production)
                           (strategy sips-search)
                           state)
  ;; Return: Boolean if finished
  (or (null (sips%context-stack state))
      (zerop (g:arity prod))))

(defmethod get-next-search-query ((prod g:production)
                                  (strategy sips-search)
                                  state)
  ;; Return: NT, index, and context
  (let ((ctx (sips%get-current-context state))
        (cix (sips.nth-child (sips%index state) (sips%sips strategy))))
    (when (null cix) (break))
    (values (nth cix (g:occurrences prod)) cix ctx)))

(defmethod search-transition ((prod g:production)
                              (strategy sips-search)
                              state
                              prog-set
                              up-info)
  ;; Return: next state
  (let ((ctx (sips%get-current-context state))
        (cix (sips.nth-child (sips%index state) (sips%sips strategy))))
    (if (listp up-info)
        (progn
          (kl:foreach (up in up-info)
            (let ((dup-ctx (make-instance 'context :copy-from ctx)))
              (setf (context.up cix dup-ctx) (car up))
              (push (make-instance 'sips-result
                                   :context dup-ctx
                                   :program-set (cdr up))
                    (nth cix (sips%results state))))))
        (progn
          (setf (context.up cix ctx) up-info)
          (push (make-instance 'sips-result
                               :context ctx
                               :program-set prog-set)
                (nth cix (sips%results state)))))
    (unless (sips%pop-current-context state)
      (when (< (incf (sips%index state)) (g:arity prod))
        (sips%populate-context-stack strategy state))))
  state)

(defmethod get-search-result ((prod g:production)
                              (strategy sips-search)
                              state)
  ;; Return: program set
  (let ((punion nil)
        (results
          (list
           (make-instance 'sips-result
                          :context (make-instance 'context
                                                  :arity (arity
                                                          (sips%sips strategy)))
                          :program-set (list)))))
    (kl:foreach (result-set in (sips%results state))
      (let ((new-results))
        (kl:foreach (res in results)
          (kl:foreach (next-result in result-set)
            (let ((merged (context.merge (sips%context next-result)
                                         (sips%context res))))
              (unless (null merged)
                (push (make-instance 'sips-result
                                     :context merged
                                     :program-set (cons (sips%program-set
                                                         next-result)
                                                        (sips%program-set
                                                         res)))
                      new-results)))))
        (setf results new-results)))
    (kl:foreach (result in results)
      (unless (= (g:arity prod) (length (sips%program-set result)))
        (format *trace-output* "~a~%" prod)
        (inspect result)
        (break)
        (error "Mismatch in program set length."))
      (push
       (combine prod (reverse (sips%program-set result)) (sips%info strategy)) 
       punion))
    (union-program-node:new punion)))

