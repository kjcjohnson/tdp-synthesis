(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:tdp)

(tdp:define-init-hook lia-witnesses
  (when (typep tdp:*algorithm* 'duet-algorithm)
    (let* ((original-examples (spec:examples
                               (semgus:specification tdp:*semgus-problem*)))
           (cap (loop for ex in original-examples
                      for os = (spec:output-state ex)
                      maximizing
                      (loop for var in (smt:get-variables os)
                            for val = (smt:get-value os var)
                            if (numberp val)
                              maximizing (abs val)
                            else
                              maximizing 0))))

      (flet ((check-range (val)
               (if (< (abs val) cap)
                   val
                   nil))
             (check-value (prev next)
               "Checks if NEXT is absolutely smaller than PREV, returning NEXT if so"
               (if (< (abs next) (abs prev))
                   next
                   nil)))

        (definv "+" 1 (in out ctx)
          (check-value out (- out (nth 0 ctx))))

        (definv "-" 1 (in out ctx)
          (check-value out (- (nth 0 ctx) out)))))))

#|
(defun witness-lia-+-1 ()
  :top)

(defun witness-lia-+-2 (input output arg-1)
  ())

(defun witness-lia-+ (down context child-ix)
  (if (zerop child-ix)
      :top
      (let ((witnessed (duet-information:copy down))
            (previous (information-context:upward-info 0)))
        )))

 out = ctx - z

 out - ctx = -z
 ctx - out = z

|#

