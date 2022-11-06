(in-package #:com.kjcjohnson.tdp.duet)
(kl/oo:import-classes-from #:tdp)

(defun check-range (val)
  (if (> (abs val) 20)
      nil
      val))

(tdp:define-init-hook lia-witnesses
  (when (typep tdp:*algorithm* 'duet-algorithm)

    (definv "+" 1 (in out ctx)
      (check-range (- out (nth 0 ctx))))
    
    (definv "-" 1 (in out ctx)
      (check-range (- (nth 0 ctx) out)))))

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
  
