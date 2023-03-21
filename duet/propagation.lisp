(in-package #:com.kjcjohnson.tdp.duet)

(defun universal-witness (prod child-ix ctx)
  (declare (ignore prod child-ix ctx))
  :top)

(defun transpose-context (context target-length)
  (let* ((ctx-list (loop for i from 0 below (tdp::arity context)
                         collecting (tdp:context.up i context))))
    (loop for i from 0 below target-length
          collecting (map 'list #'(lambda (s)
                                    (let ((val (nth i s)))
                                      (if (null val)
                                          val
                                          (smt:get-first-value val))))
                          ctx-list))))

(defun set-new-info (info output-spec)
  "Sets information on a duet information object"
  (if (find t output-spec :key #'(lambda (os) (typep os 'duet-refinement)))
      (setf (duet-information:refinement info)
            (map 'list
                 #'(lambda (r)
                     (if (typep r 'duet-refinement)
                         r
                         (make-instance 'duet-refinement
                                        :name 'true
                                        :refinement-function (constantly t))))
                 output-spec)
            (duet-information:outputs info)
            nil)
      (setf (duet-information:outputs info)
            (map 'list
                 #'(lambda (s) (smt:make-state (list :output s)))
                 output-spec))))

(defun zip-removing-null (lists)
  (remove-if #'(lambda (x) (some #'null x))
             (apply #'map 'list #'list lists)))

(defmethod tdp:infer ((prod g:production)
                      child-ix
                      (outer-spec duet-information)
                      context)
  (let* ((pn (map-production-to-witness-name prod))
         (info (duet-information:copy outer-spec))
         (inv (get-inverse-semantics pn child-ix)))
    
    (setf (duet-information:check-library? info) t)
    (cond
      ((not (null inv))
       (let* ((trans-ctx (transpose-context context
                                            (length
                                             (duet-information:outputs info))))
              (witnessed
                (map 'list #'(lambda (x)
                               (if (listp x)
                                   x ;(list (first x))#| HERE |#
                                   (list x)))
                     (map 'list
                          #'(lambda (in out ctx)
                              (funcall inv
                                       in
                                       (smt:get-first-value out)
                                       ctx :production prod))
                          (duet-information:inputs info)
                          (duet-information:outputs info)
                          trans-ctx)))
              ;;(combos (tdp:all-cart-prod witnessed))) ; This is suspect for replace
              (combos (zip-removing-null witnessed)))

         (cond
           ((null combos)
            (setf info nil))

           ((find :top (map 'list #'(lambda (x)
                                      (find :top x))
                            combos))
            (setf info (universal-witness prod child-ix outer-spec)))
           
           ((= 1 (length combos))
            (set-new-info info (first combos)))

           (t
            (warn "---Disjunctive Spec---:~a (~a combinations)~%"
                  pn (length combos))
            (loop with sub-specs = nil
                  for combo in combos
                  for sub-info = (duet-information:copy info)
                  doing
                     (set-new-info sub-info combo)
                     (push sub-info sub-specs)
                  finally
                     (setf info (disjunctive-duet-information:new sub-specs))))))
       
       info)

      (t
       (universal-witness prod child-ix outer-spec)))))

(defmethod tdp:derive ((prod g:production)
                       child-ix
                       program-set
                       (outer-spec duet-information)
                       context)
  (let (up)
    (vsa:do-programs (program program-set)
      (let ((descriptors (ast:semantics-descriptors-for-non-terminal
                          tdp:*semantics*
                          (g:instance (ast:production program)))))
        (when (> (length descriptors) 1)
          (error "Cannot derive when multiple matching descriptors"))
        (push
         (cons
          (map 'list #'(lambda (input)
                         (ast:execute-program tdp:*semantics*
                                              (first descriptors)
                                              program
                                              input))
               (duet-information:inputs outer-spec))
          (make-instance 'vsa:leaf-program-node :program program))
       up)))
    up))

    ;(make-instance 'duet-up-info
    ;               :programs programs
    ;               :outputs outputs)))
