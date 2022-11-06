(in-package #:com.kjcjohnson.tdp.test)

(defun benchmark-solver (solver-fn problem)
  "Runs a solver on a problem and computes stats."
  (let ((start-time (get-universal-time))
        (end-time nil))
    (unwind-protect
         (format t "Result: ~a~%" (funcall solver-fn problem))
      (setf end-time (- (get-universal-time) start-time)))
    (format t "Time: ~a s~%" end-time)
    end-time))

(defun benchmark-problem (problem)
  "Runs multiple solvers on a problem."
  (let ((fns (list ;#'enum::enum-solve "Enum"
                   ;#'duet::duet-solve "Duet"
                   #'random-duet-solve "Random Duet"
                   ;#'frangel::fragment-search "FrAngel"
                   ;#'frangel::random-search "Random"
                   )))
    (loop for (fn name) on fns by #'cddr
          collecting
          (progn
            (format t "Solver: ~a~%" name)
            (handler-case
                (sb-ext:with-timeout 60
                  (benchmark-solver fn problem))
              (sb-ext:timeout ()
                (format t "Timeout~%")
                nil))))))
    
