;;;
;;; Common functionality for benchmark usage
;;;
(in-package #:com.kjcjohnson.tdp.test)

(defun grab-p (grammar name)
  (find-if #'(lambda (p) (string= (g:name p) name)) (g::productions grammar)))

(defun grab-op (grammar name)
  (g:operator (grab-p grammar name)))

(defun alist-update (key value alist)
  "Copies the given alist and replaces the given value. Returns the copied list."
  (setf alist (copy-alist alist))
  (rplacd (assoc key alist) value)
  alist)
