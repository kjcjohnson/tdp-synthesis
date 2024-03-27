;;;;
;;;; New program record implementation - hopefully better
;;;;
(in-package #:com.kjcjohnson.tdp.top-down-enum)

(defstruct (shin-program-entry (:conc-name pe-))
  "New-style program entry on the queue"
  program size filled-trace hole-traces hole-nts)

(defun pe-has-hole? (pe)
  (declare (type shin-program-entry pe))
  (not (null (pe-hole-traces pe))))

(declaim (inline %copy-replacing-child))
(defun %copy-replacing-child (program index new-child)
  "Copies a program node and replaces a child"
  (make-instance 'ast:program-node
                 :production (ast:production program)
                 :operator (ast:operator program)
                 :children (let ((children (copy-list (ast:children program))))
                             (setf (elt children index) new-child)
                             children)))

(defun %generate-new-traces (current frag-traces)
  "Generates new hole traces for fragment traces FRAG-TRACES, based on CURRENT trace"
  (loop with curr-len = (length current)
        for ft in frag-traces
        for frag-len = (length ft)
        for new-arr = (make-array (+ curr-len frag-len) :element-type 'fixnum)
        do (loop for i from 0 below curr-len
                 do (setf (aref new-arr i) (aref current i)))
           (loop for i from 0 below frag-len
                 do (setf (aref new-arr (+ i curr-len)) (elt ft i)))
        collect new-arr))

;;;
;;; Fragments - instead of filling holes with just a production and new holes, we'll
;;;   build a set of fragments for each non-terminal. These are a pre-built node
;;;   with holes such that 1) we re-use the same holes everywhere; 2) we can iterate
;;;   over the fragments instead of going back to the grammar; 3) we can pre-fill
;;;   holes where there's only one possible production.
;;;
(defstruct (shin-fragment (:conc-name shfrag-))
  "New-style fragments for building programs"
  program size traces nts)

(defun %build-hole-set (grammar)
  "Builds a set of holes for non-terminals in GRAMMAR"
  (let ((hole-hash (make-hash-table)))
    (g:do-non-terminals (nt grammar)
      (setf (gethash nt hole-hash)
            (make-instance 'ast:program-hole :non-terminal nt)))
    hole-hash))

(defun %build-fragments (grammar holes)
  "Builds a set of fragments for GRAMMAR"
  (labels ((build-fragment (prod &optional initial-prod)
             "Builds a fragment from PROD"
             (loop for occ in (g:occurrences prod)
                   for cprods = (g:productions-for-instance grammar occ)
                   if (and (= 1 (length cprods))
                           (not (eql prod initial-prod))) ; Circular grammars...
                     collect (build-fragment (first cprods) (or initial-prod prod))
                       into child-nodes
                   else
                     collect (gethash occ holes)
                       into child-nodes
                   finally (return (make-instance 'ast:program-node
                                                  :children child-nodes
                                                  :production prod))))
           (collect-nts-and-traces (frag &optional curr-trace)
             "Collects a list of "
             (if (typep frag 'ast:program-hole)
                 (list (cons (ast:non-terminal frag) curr-trace))
                 (loop for i from 0 below (g:arity (ast:production frag))
                       appending (collect-nts-and-traces (ast:nth-child i frag)
                                                         (cons i (reverse curr-trace)))))))

    (let ((frag-hash (make-hash-table)))
      (g:do-non-terminals (nt grammar)
        (loop for prod in (g:productions-for-instance grammar nt)
              for frag = (build-fragment prod)
              for size = (ast:program-size frag)
              for nts-and-traces = (collect-nts-and-traces frag)
              collect (make-shin-fragment :program frag
                                          :size size
                                          :nts (map 'list #'car nts-and-traces)
                                          :traces (map 'list #'cdr nts-and-traces))
                into fragments
              finally (setf (gethash nt frag-hash) fragments)))
      frag-hash)))

;;;
;;; Initialization
;;;
(defun get-initial-entry (grammar)
  "Generates the initial program entry"
  (let* ((holes (%build-hole-set grammar))
         (frags (%build-fragments grammar holes)))
    (values
     (make-shin-program-entry
      :program (gethash (g:initial-non-terminal grammar) holes)
      :size 1
      :hole-traces (list (make-array 0 :element-type 'fixnum))
      :hole-nts (list (g:initial-non-terminal grammar)))
     frags)))

(defun expand-next-hole (pe fragments)
  (declare (type shin-program-entry pe))
  (let ((next-trace (pop (pe-hole-traces pe)))
        (next-nt (pop (pe-hole-nts pe))))
    (labels ((replace-trace (node tix frag)
               (if (typep node 'ast:program-hole)
                   frag
                   (let ((child-ix (aref next-trace tix)))
                     (%copy-replacing-child node
                                            child-ix
                                            (replace-trace
                                             (ast:nth-child child-ix node)
                                             (1+ tix)
                                             frag))))))
      (loop for frag in (gethash next-nt fragments)
            for root = (replace-trace (pe-program pe) 0 (shfrag-program frag))
            collecting (make-shin-program-entry
                        :program root
                        :size (+ (pe-size pe) (shfrag-size frag) -1) ; Subtract hole
                        :hole-traces (nconc (%generate-new-traces next-trace (shfrag-traces frag))
                                            (pe-hole-traces pe))
                        :hole-nts (append (shfrag-nts frag)
                                          (pe-hole-nts pe)))
              into pes
            finally (return pes)))))
