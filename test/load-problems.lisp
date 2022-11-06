;;;;
;;;; Loads problem files
;;;;
(in-package :com.kjcjohnson.tdp.test)

(defparameter *path-base* #+win32 "d:/temp/benchmarks/classic/" #-win32 "/mnt/d/temp/")

(defun load-problem (name)
  (declare (ignore name))
  #|(com.kjcjohnson.synthkit.semgus:load-semgus-problem
   #+win32 (format nil "d:/temp/benchmarks/classic/~a.sexpr" name)
   #-win32 (format nil "/mnt/d/temp/~a.sexpr" name)))|# nil)

(defparameter *drn* (load-problem "dr-name"))

(defparameter *bks* (load-problem "bikes"))

(defparameter *bkl* (load-problem "bikes-long"))

(defparameter *11440431* (load-problem "11440431"))

(defparameter *ini* (load-problem "initials"))

(defparameter *inl* (load-problem "initials-long"))

(defparameter *ezr* (load-problem "easy_r"))

(defparameter *ezs* (load-problem "easy_s"))

(defparameter *ph9* (load-problem "phone-9"))

(defparameter *uv1* (load-problem "univ_1"))

(defparameter *uv2* (load-problem "univ_2"))

(defparameter *pre* (load-problem "PRE_1_10"))

(defparameter *bve* (load-problem "bv-easy"))

(defparameter *prz* (load-problem "PRE_ez"))
