(tdp::tdp (make-instance 'enum::enumerator-algorithm) *max2-exp-g* *max2-exp-sem* (make-instance 'enum:enumerator-info :prune t :max-depth 4 :inputs '(((:x . 5) (:y . 4)) ((:x . 7) (:y . 9))) :outputs '(5 9)))

(com.kjcjohnson.kale.oo:import-classes-from #:duet)
(defparameter *duet-info* (duet-information:new))
(setf (duet-information:check-library? *duet-info*) t)
(setf (duet-information:outputs *duet-info*) '(5 9))
(setf (duet-information:inputs *duet-info*) '(((:x . 5) (:y . 4)) ((:x . 7) (:y . 9))))

(tdp::tdp (make-instance 'duet::duet-algorithm) *max2-exp-g* *max2-exp-sem* *duet-info*)
