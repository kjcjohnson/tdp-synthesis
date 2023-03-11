;;;;
;;;; System definition for top-down synthesis algorithmic framework
;;;;
(asdf:defsystem "com.kjcjohnson.tdp"
  :description "Framework for top-down propagation synthesis"
  :version "0.0.1"
  :author "Keith Johnson <keith.johnson@wisc.edu>"
  :license "TBD"
  :depends-on ("com.kjcjohnson.tdp/main"
               "com.kjcjohnson.tdp/enumerative"
               "com.kjcjohnson.tdp/duet"
               "com.kjcjohnson.tdp/top-down-enum"))

(asdf:defsystem "com.kjcjohnson.tdp/main"
  :depends-on ("com.kjcjohnson.synthkit"
               "com.kjcjohnson.kale")
  :components ((:file "package")
               (:file "globals" :depends-on ("package"))
               (:module "infrastructure" :depends-on ("package" "globals")
                :serial t
                :components ((:file "synthesis-tasks")
                             (:file "default-tasks")
                             (:file "context")
                             (:file "sips")
                             (:file "sips-search")
                             (:file "utility")))))

(asdf:defsystem "com.kjcjohnson.tdp/enumerative"
  :depends-on ("com.kjcjohnson.tdp/main")
  :pathname "enumerative"
  :serial t
  :components ((:file "package")
               (:file "algorithm")
               (:file "enumerator-info")
               (:file "combination")
               (:file "propagation")
               (:file "filter-task")
               (:file "search")
               (:file "main")))

(asdf:defsystem "com.kjcjohnson.tdp/duet"
  :depends-on ("com.kjcjohnson.tdp/main"
               "com.kjcjohnson.tdp/enumerative"
               "str")
  :pathname "duet"
  :serial t
  :components ((:file "package")
               (:file "algorithm")
               (:file "information")
               (:file "combination")
               (:file "component-library")
               (:file "disjunctive-information")
               (:file "dispatcher")
               (:file "witnesses")
               (:file "propagation")
               (:file "lia-witnesses")
               (:file "string-witnesses")
               (:file "main")))

(asdf:defsystem "com.kjcjohnson.tdp/top-down-enum"
  :depends-on ("com.kjcjohnson.tdp/main"
               "priority-queue")
  :pathname "top-down-enum"
  :serial t
  :components ((:file "package")
               (:file "info")
               (:file "algorithm")
               (:file "tasks")
               (:file "main")))

(asdf:defsystem "com.kjcjohnson.tdp/test"
  :depends-on ("com.kjcjohnson.tdp"
               "str"
               "com.kjcjohnson.frangel")
  :pathname "test"
  :components ((:file "package")
               (:file "enumerator")
               (:file "random-duet")
               (:file "frangel-duet")
               (:file "pp-infra")
               (:file "load-problems")
               (:file "benchmarking")
               (:module "benchmarks"
                :pathname "../benchmarks"
                :components ((:file "max2-exp")
                             (:file "dr-name")))))

(asdf:defsystem "com.kjcjohnson.tdp/ks2"
  :depends-on ("com.kjcjohnson.tdp"
               "com.kjcjohnson.frangel"
               "com.kjcjohnson.ks2/solver-api")
  :pathname "ks2"
  :serial t
  :components ((:file "package")
               (:file "utility")
               (:file "top-down-enum")
               (:file "duet")
               (:file "frangel")
               (:file "bottom-up-enum")))
