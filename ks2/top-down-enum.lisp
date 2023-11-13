;;;;
;;;; Top-down enumerator ks2 integration
;;;;
(in-package #:com.kjcjohnson.tdp.ks2.solver-api)

(register-solver :top-down-enum 'top-down-enumerator)

(defclass top-down-enumerator (s-api:solver)
  ()
  (:documentation "The top-down enumerator solver class"))

(define-solver-metadata top-down-enumerator
  :name "Top-down Enumerator"
  :symbol ("tde" "top-down-enum")
  :description "A standard top-down enumerative solver"
  :action "TDE Solve"
  :spec-transformer #'%io-cegis-rel-spec-transformer
  :options (list
            (s-api:make-solver-option
             :keyword :defer-prune
             :name "Defer Prune"
             :description "Whether or not pruning candidates happens at dequeue"
             :type :boolean
             :default t)
            (s-api:make-solver-option
             :keyword :prune-stats
             :name "Collect Pruning Statistics"
             :description "Whether to collect pruning statistics and report them"
             :type :boolean)
            (s-api:make-solver-option
             :keyword :new-queue
             :name "Use the New Queue Implementation"
             :description "Use a new priority queue implementation"
             :default nil
             :type :boolean)
            (s-api:make-solver-option
             :keyword :queue
             :name "Queue implementation to use"
             :description "Selects a particular queue implementation to use"
             :default :pq
             :type '(:member :pq :new :df :dfs))
            (s-api:make-solver-option
             :keyword :prune-max-holes
             :name "Prune Max Holes"
             :description "Maximum number of holes in a program to consider pruning"
             :type :number)
            (s-api:make-solver-option
             :keyword :prune-min-holes
             :name "Prune Min Holes"
             :description "Minimum number of holes in a program to consider pruning"
             :type :number)
            (s-api:make-solver-option
             :keyword :no-prune-1
             :name "Don't prune certain productions (1)"
             :description "Doesn't prune certain productions when filled in"
             :type :boolean
             :default t)
            (s-api:make-solver-option
             :keyword :no-prune-2
             :name "Don't prune certain productions (2)"
             :description "Doesn't prune certain production patterns of size 2"
             :type :boolean
             :default t)
            (s-api:make-solver-option
             :keyword :overhead-only
             :name "Measure overhead only"
             :description "Only run overhead and don't remove pruned programs"
             :type :boolean
             :default nil)
            (s-api:make-solver-option
             :keyword :initial-queue-size
             :name "Initial queue size"
             :description "Initial storage size of the queue, if supported by the queue"
             :type :number)))

(defmethod solve-problem ((solver top-down-enumerator) semgus-problem
                          &key (defer-prune t) prune-stats new-queue queue
                            generate-interval-semantics
                            prune-max-holes prune-min-holes
                            (no-prune-1 nil) (no-prune-2 nil)
                            (overhead-only nil) (trace nil)
                            initial-queue-size)
  (setf tde::*defer-prune* defer-prune)
  (setf tde::*collect-prune-stats* prune-stats)
  (setf tde::*should-prune-hook* #'(lambda (program)
                                     (if (or prune-max-holes prune-min-holes)
                                         (let ((hc (ast:hole-count program)))
                                           (and
                                            (or (not prune-max-holes)
                                                (<= hc prune-max-holes))
                                            (or (not prune-min-holes)
                                                (>= hc prune-min-holes))))
                                         t)))
  (setf tde::*use-new-pq* new-queue)
  (setf tde::*no-prune-1* no-prune-1)
  (setf tde::*no-prune-2* no-prune-2)
  (setf tde::*remove-pruned* (not overhead-only))
  (setf tde::*pq-impl* queue)
  (setf tde::*initial-queue-size* initial-queue-size)
  (maybe-trace (semgus-problem
                (format nil
                        "OH_~a_GI_~a_Q_~a"
                        overhead-only
                        generate-interval-semantics
                        queue)
                trace)
    (semgus:maybe-with-cegis (solver semgus-problem)
      (tde:top-down-enum-solve semgus-problem))))
