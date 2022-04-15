(in-package #:com.kjcjohnson.tdp)

#|

Basic strategy:

1. Compute a precedence list of indices

2. Start from the 1st index and generate a program set

3. For each distinct value (in upward info), move on to next index 


|#

(kl/oo:define-encapsulated-class ltr-query

    ;; Constructor
    (public constructor (index non-terminal information)
            (setf this.index index
                  this.non-terminal non-terminal
                  this.information information))
  
  (public field index)
  (public field non-terminal)
  (public field information))

;;;
;;; Left-to-right search strategy with dependencies
;;;
(kl/oo:define-encapsulated-class ltr-search-strategy

  (private field _precedence-list)
  (private field _dependency-list)
  (private field _production)
  (private field _current-index)
  (private field _downward-info)
  
  (public constructor (prod downward-info)
          ;; For now, just do left-to-right
          (setf _precedence-list (loop for i from 0 below (g:arity prod)
                                       collecting i))
          (setf _dependency-list (loop for i in _precedence-list
                                       doing
                                          (loop for j from 0 below i
                                                collecting j)))
          (setf _current-index 0)
          (setf _production prod)
          (setf _downward-info downward-info)
          )
  
  (public get-next-query ()
          (let* ((nt-ix (nth _current-index _precedence-list))
                 (nt (nth nt-ix (g:occurrences _production))))
            (if (zerop _current-index)
                (ltr-query:new nt-ix nt _downward-info)
                
                      
