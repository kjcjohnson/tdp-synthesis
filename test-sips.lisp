(in-package #:com.kjcjohnson.tdp.test)

(defparameter *nt-a* (make-instance 'g:non-terminal :name "A"))
(defparameter *nt-b* (make-instance 'g:non-terminal :name "B"))
(defparameter *nt-t* (make-instance 'g:non-terminal :name "T"))

(defparameter *p* (make-instance 'g:production
                                 :occurrences (list *nt-a* *nt-b*)
                                 :operator (make-instance 'g:operator
                                                          :arity 2
                                                          :name "Test")
                                 :name "Test"
                                 :instance *nt-t*))
