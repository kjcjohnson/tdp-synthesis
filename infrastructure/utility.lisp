(in-package #:com.kjcjohnson.tdp)

(defun ensure-list (maybe-list)
  "Ensures that the given argument is a list. Returns MAYBE-LIST if it is a list,
otherwise returns a single-element list with MAYBE-LIST as the only value."
  (typecase maybe-list
    (list maybe-list)
    (otherwise (list maybe-list))))

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
                                              (car list))
                                         output)))
            (all-cart-prod (cdr list)))
       output))))
