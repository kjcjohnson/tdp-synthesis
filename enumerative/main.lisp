;;;;
;;;; Main enumerator entrance
;;;;
(in-package #:com.kjcjohnson.tdp.enumerative)

(defun enum-solve* (semgus-problem depth &optional (prune t))
  "Enumerates all programs to the given DEPTH"
  (tdp::tdp-s semgus-problem
              (make-instance 'enumerator-algorithm)
              (make-instance 'enumerator-info
                             :prune prune
                             :max-depth depth
                             :inputs (map 'list
                                          #'semgus:example-input
                                          (semgus:examples
                                           (semgus:specification semgus-problem)))
                             :outputs (map 'list
                                           #'semgus:example-output
                                           (semgus:examples
                                            (semgus:specification semgus-problem))))))

(defun enum-solve (semgus-problem &key (max-depth 8) (prune t))
  "Searches enumeratively for a program breadth-first, halting after MAX-DEPTH is searched. Returns the smallest matching program at a given depth."
  (loop for depth from 1 to max-depth
        for programs = (enum-solve* semgus-problem depth prune)
        when programs return it))
