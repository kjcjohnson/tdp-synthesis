;;;;
;;;; Infrastructure for perfect propagation programs
;;;;
(in-package #:com.kjcjohnson.tdp.test)

;;;
;;; Handle constants - this should probably be in main synthkit library
;;;
(defclass constant-program-node (ast:program-node)
  ((value :reader node-value :initarg :value)))

(defmethod ast:operational-semantics-for-production ((sem semgus::default-semantics)
                                                     prod
                                                     (node constant-program-node))
  "Returns the value of a constant program node."
  (list #'(lambda (is)
            (declare (ignore is))
            (node-value node))))

(defmethod ast::print-program-node ((node constant-program-node) stream)
  "Pretty-prints a constant program node."
  (format stream "~s" (node-value node)))

;;;
;;; Basic support objects - the PP algorithm and information classes
;;;
(defclass pp-algo ()
  ((witness :reader witness-functions :initarg :witnesses)))

(defclass pp-info ()
  ((input :reader input :initarg :input)
   (output :reader output :initarg :output)))

(defmethod print-object ((obj pp-info) stream)
  "Pretty-prints a perfect propagation information object."
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~s ~~~~> ~s" (input obj) (output obj))))

;;;
;;; Dispatcher - use the regular propagation for everything except constants
;;;
(defmethod tdp:synthesize-dispatch ((algo pp-algo) nt-or-prod info)
  "Dispatcher for perfect propagation."
  (cond
    ((null info)
     nil)
    ((null (output info))
     nil)
    (t
     (if (string= (g:name nt-or-prod) "Constant")
         :constant
         :tdp))))

(defmethod tdp::synthesize* ((obj (eql :constant)) prod info)
  "Specialized synthesis task for constants."
  (if (consp (output info))
      nil
      (list (make-instance 'constant-program-node
                           :value (output info)
                           :production prod))))

;;;
;;; Inference - call our witness functions
;;;
(defmethod tdp::infer ((prod g:production) child-index outer-spec context)
  (declare (ignore context))
  (let ((prod-name (g:name prod))
        (output (output outer-spec)))
    (if (and
         (not (consp output))
         (string= prod-name "Constant")
         (= 0 child-index))
        
        output
    
        (make-instance 'pp-info
                       :input (input outer-spec)
                       :output (funcall (witness-functions tdp::*algorithm*)
                                        prod-name
                                        child-index
                                        output)))))

(defmethod tdp::derive ((prod g:production)
                        child-index
                        prog-set
                        outer-spec
                        context-info)
  (declare (ignore prod child-index prog-set outer-spec context-info))
  nil)

;;;
;;; Combination and utilities
;;;
(defun all-cart-prod (list)
  "Computes the cartesian product of a list of lists."
  (cond
    ((endp list)
     nil)
    ((= 1 (length list))
     (map 'list #'list (car list)))
    (t
     (let ((output (list)))
       (map nil #'(lambda (n)
                    (setf output (append (map 'list #'(lambda (r)
                                         (cons r n))
                               (car list)) output)))
            (all-cart-prod (cdr list)))
       output))))

(defmethod tdp::combine (prod children context-info)
  "Default combination function. Finds all valid combinations of children that produce the output."
  (break)
  (let ((nodes (map 'list #'(lambda (children)
                              (make-instance 'ast:program-node :children children :production prod))
                    (if (zerop (g:arity prod))
                        (list nil)
                        (all-cart-prod children)))))

    (if (typep context-info 'pp-info)
        (delete-if-not
         #'(lambda (pro)
             (equal (ast:execute-program tdp::*semantics* pro (input context-info))
                    (output context-info)))
         nodes)
        nodes)))

;;;
;;; Toplevel convenience function
;;;
(defun pp-solve (grammar semantics witnesses &key input output)
  (let ((programs
          (tdp::tdp (make-instance 'pp-algo :witnesses witnesses)
                    grammar
                    semantics
                    (make-instance 'pp-info :input input :output output))))
    (format t "Found the following programs: ~%")
    (dolist (prog programs)
      (format t "~a~%" prog))
    programs))
    
