(in-package #:com.kjcjohnson.tdp)

;;;
;;; Contextual information - downward and upward information
;;;
(defclass context ()
  ((downward-info
    :documentation "Downward-flowing information used to synthesize child")
   (upward-info
    :documentation "Derived information from a child's synthesis run")
   (arity
    :reader arity
    :documentation "Number of children in this context"))
  (:documentation "Contextual information about a production's synthesis"))

(defmethod initialize-instance :after ((ctx context)
                                       &key arity copy-from
                                       &allow-other-keys)
  "Initializes a context with the given arity"
  (if (null copy-from)
      (setf (slot-value ctx 'downward-info)
            (make-list arity)
            (slot-value ctx 'upward-info)
            (make-list arity)
            (slot-value ctx 'arity)
            arity)
      (setf (slot-value ctx 'downward-info)
            (copy-list (slot-value copy-from 'downward-info))
            (slot-value ctx 'upward-info)
            (copy-list (slot-value copy-from 'upward-info))
            (slot-value ctx 'arity)
            (arity copy-from))))

(defgeneric context.up (n context)
  (:documentation "Gets the upward information for the nth child")
  (:method (n (context context))
    (assert (< n 10))
    (nth n (slot-value context 'upward-info))))

(defgeneric (setf context.up) (value n context)
  (:documentation "Sets the upward information for the nth child")
  (:method (value n (context context))
    (assert (< n 10))
    (setf (nth n (slot-value context 'upward-info)) value)))

(defgeneric context.down (n context)
  (:documentation "Gets the downward information for the nth child")
  (:method (n (context context))
    (assert (< n 10))
    (nth n (slot-value context 'downward-info))))

(defgeneric (setf context.down) (value n context)
  (:documentation "Sets the downward information for the nth child")
  (:method (value n (context context))
    (assert (< n 10))
    (setf (nth n (slot-value context 'downward-info)) value)))

(defgeneric context.information-equivalent? (info1 info2)
  (:documentation "Checks if the two information objects are equivalent.")
  (:method (info1 info2)
    (equal info1 info2)))

(defgeneric context.merge-information (info1 info2)
  (:documentation "Merges two information objects if possible. Returns the merged
information as the first value, and NIL as the second if cannot merge.")
  (:method (info1 info2)
    (cond
      ((context.information-equivalent? info1 info2)
       (values info1 t))
      ((or (null info1) (null info2))
       (values (or info1 info2) t))
      (t
       (values nil nil)))))

(defgeneric context.merge (context1 context2)
  (:documentation "Merges the two contexts, or returns NIL if not mergeable")
  (:method ((context1 context) (context2 context))
    (if (= (arity context1) (arity context2))
        (loop
          with merged-context = (make-instance 'context :arity (arity context1))
          for ix from 0 below (arity context1)
          for (down downp) = (multiple-value-list (context.merge-information
                                                   (context.down ix context1)
                                                   (context.down ix context2)))
          for (up upp) = (multiple-value-list (context.merge-information
                                               (context.up ix context1)
                                               (context.up ix context2)))
          unless (and downp upp) return nil
            doing (setf (context.down ix merged-context) down
                        (context.up ix merged-context) up)
          finally (return merged-context))
        nil)))
              
