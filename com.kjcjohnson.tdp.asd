;;;;
;;;; System definition for top-down synthesis algorithmic framework
;;;;
(asdf:defsystem "com.kjcjohnson.tdp"
  :description "Framework for top-down propagation synthesis"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :depends-on ("com.kjcjohnson.synthkit"
               "com.kjcjohnson.synthkit/semgus/interop"
               "com.kjcjohnson.kale")
  :components ((:file "package")
               (:file "globals" :depends-on ("package"))
               (:file "synthesis-tasks" :depends-on ("package" "globals"))))

(asdf:defsystem "com.kjcjohnson.tdp/test"
  :depends-on ("com.kjcjohnson.tdp")
  :components ((:file "enumerator")
               (:file "pp-infra")
               (:module "benchmarks"
                :components ((:file "max2-exp")))))
