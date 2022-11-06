;;;;
;;;; Witness functions for strings
;;;;
(in-package #:com.kjcjohnson.tdp.duet)

;;;
;;; Alignment helpers for replacement
;;;
(defun alignment-score (a b a-ix b-ix)
  "Computes alignment score for two characters"
  (if (char= (aref a (1- a-ix))
             (aref b (1- b-ix)))
      1
      -1))

#|
d <-- Gap penalty score
for i = 0 to length(A)
    F(i,0) <-- d * i
for j = 0 to length(B)
    F(0,j) <-- d * j
for i = 1 to length(A)
    for j = 1 to length(B)
    {
        Match <-- F(i-1, j-1) + S(Ai, Bj)
        Delete <-- F(i-1, j) + d
        Insert <-- F(i, j-1) + d
        F(i,j) <-- max(Match, Insert, Delete)
    }
|#

(defun compute-f-matrix (a b gap-penalty)
  "Computes an alignment matrix for A and B"
  (declare (string a b))
  (let ((f (make-array (list (1+ (length a)) (1+ (length b))))))
    (loop for i from 0 to (length a)
          doing (setf (aref f i 0) (* gap-penalty i)))
    (loop for j from 0 to (length b)
          doing (setf (aref f 0 j) (* gap-penalty j)))
    (loop for i from 1 to (length a) doing
      (loop for j from 1 to (length b)
            for match = (+ (aref f (1- i) (1- j))
                           (alignment-score a b i j))
            for delete = (+ (aref f (1- i) j) gap-penalty)
            for insert = (+ (aref f i (1- j)) gap-penalty)
            doing (setf (aref f i j)
                        (max match delete insert))))
    f))
            
#|
AlignmentA <-- ""
AlignmentB <-- ""
i <-- length(A)
j <-- length(B)
while (i > 0 or j > 0)
{
    if (i > 0 and j > 0 and F(i, j) == F(i-1, j-1) + S(Ai, Bj))
    {
        AlignmentA <-- Ai + AlignmentA
        AlignmentB <-- Bj + AlignmentB
        i <-- i - 1
        j <-- j - 1
    }
    else if (i > 0 and F(i, j) == F(i-1, j) + d)
    {
        AlignmentA <-- Ai + AlignmentA
        AlignmentB <-- "-" + AlignmentB
        i <-- i - 1
    }
    else
    {
        AlignmentA <-- "-" + AlignmentA
        AlignmentB <-- Bj + AlignmentB
        j <-- j - 1
    }
}
|#
(defun compute-alignment (a b f gap-penalty gap-character)
  "Computes alignment strings for A and B"
  (let ((alignment-a nil)
        (alignment-b nil)
        (i (length a))
        (j (length b)))
    (loop while (or (> i 0) (> j 0)) doing
      (cond
        ((and (> i 0)
              (> j 0)
              (= (aref f i j)
                 (+ (aref f (1- i) (1- j))
                    (alignment-score a b i j))))
         (push (aref a (1- i)) alignment-a)
         (push (aref b (1- j)) alignment-b)
         (decf i)
         (decf j))

        ((and (> i 0)
              (= (aref f i j)
                 (+ (aref f (1- i) j)
                    gap-penalty)))
         (push (aref a (1- i)) alignment-a)
         (push gap-character alignment-b)
         (decf i))

        (t
         (push gap-character alignment-a)
         (push (aref b (1- j)) alignment-b)
         (decf j))))
    (values (coerce alignment-a 'string)
            (coerce alignment-b 'string))))

(defun alignment-edit-distance (alignment-a alignment-b gap-character)
  "Computes edit distance between two alignments"
  (reduce #'+ (map 'list
                   #'(lambda (a b)
                       (cond
                         ((char= a b)
                          0)
                         ((or (char= gap-character a)
                              (char= gap-character b))
                          2)
                         (t
                          1)))
                   alignment-a
                   alignment-b)))

(defun alignment-strings (a b &key (gap-character #\-))
  "Gets alignments for strings A and B."
  (let ((f (compute-f-matrix a b -1)))
    (multiple-value-bind (alignment-a alignment-b)
        (compute-alignment a b f -1 gap-character)
      (values alignment-a
              alignment-b
              (alignment-edit-distance alignment-a alignment-b gap-character)))))

(defun replace-with-rep-pairs (s p)
  (let ((u s))
    (loop for pair in p doing
      (setf u (str:replace-first (car pair) (cdr pair) u)))
    u))

(defun get-src (a b)
  (let ((p (get-rep-pairs a b)))
    (if (null p)
        nil
        (replace-with-rep-pairs a (butlast p)))))

(defun get-rep-pairs (a b)
  (let ((p nil))
    (multiple-value-bind (alignment-a alignment-b edit-distance)
        (alignment-strings a b :gap-character #\Null)
      (unless (>= edit-distance (min (length a) (length b)))
        (let ((a-sub nil)
              (b-sub nil))
          ;(format *trace-output* ";AA: ~s; AB: ~s~%" alignment-a alignment-b)
          (loop for i from 0 to (length alignment-a) doing
            (if (or
                 (= i (length alignment-a))
                 (char= (aref alignment-a i) (aref alignment-b i)))
                (progn
                  (push (cons (coerce (nreverse (delete-if #'(lambda (c)
                                                               (char= c #\Null))
                                                           a-sub))
                                      'string)
                              (coerce (nreverse (delete-if #'(lambda (c)
                                                               (char= c #\Null))
                                                           b-sub))
                                      'string))
                        p)
                  (setf a-sub nil)
                  (setf b-sub nil))
                (progn
                  (push (aref alignment-a i) a-sub)
                  (push (aref alignment-b i) b-sub)))))))
    (setf p (nreverse
             (delete-if #'(lambda (c)
                            (and (string= "" (car c))
                                 (string= "" (cdr c))))
                        
                        p)))
    ;(format *trace-output* ";GRP: ~s~%" p)
    (if (string= b (replace-with-rep-pairs a p))
        p
        nil)))
                  

;;;
;;; Witness definitions here
;;;
(tdp:define-init-hook string-witnesses
  (when (typep tdp:*algorithm* 'duet-algorithm)

    (definv "identity" 0 (in out ctx)
      out)
    
    (definv "Start" 0 (in out ctx)
      out)

    (definv "str.++" 0 (in out ctx)
      (declare (string out))
      (make-instance 'duet-refinement
                     :name 'str.++-0
                     :refinement-function #'(lambda (input)
                                              (let ((mm (mismatch input out)))
                                                (or (null mm)
                                                    (> mm 0))))))
    
    (definv "str.++" 1 (in out ctx)
      (let ((prefix (nth 0 ctx)))
        (declare (string prefix))
        (if (str:starts-with? prefix out)
            (str:substring (length prefix) t out)
            nil)))

    (definv "str.substr" 0 (in out ctx)
      (declare (string out))
      (make-instance 'duet-refinement
                     :name 'str.substr-0
                     :refinement-function #'(lambda (input)
                                              (str:contains? out input))))
    
    (definv "str.substr" 1 (in out ctx)
      (let ((src (nth 0 ctx)))
        (declare (string src))
        (first
         (loop with pos = -1
               until (null pos)
               for start = 0 then (1+ pos)
               while (< pos (length src))
               doing (setf pos (search out src :start2 start))
               unless (null pos) collecting pos))))

    (definv "str.substr" 2 (in out ctx)
      (let ((start (nth 1 ctx)))
        (declare (string out) (integer start))
        (+ start (length out))))

    (definv "str.at" 0 (in out ctx)
      (declare (type (or string null) out))
      (if (= 1 (length out))
          (make-instance 'duet-refinement
                         :name 'str.at-0
                         :refinement-function #'(lambda (input)
                                                  (str:contains? out input)))
          nil))
    
    ;; Note that this is identical to substr, as "characters" are single strings
    (definv "str.at" 1 (in out ctx) ;; TODO: make into refinement witness
      (let ((src (nth 0 ctx)))
        (declare (string src))
        (loop with pos = -1
              until (null pos)
              for start = 0 then (1+ pos)
              while (< pos (length src))
              doing (setf pos (search out src :start2 start))
              unless (null pos) collecting pos)))

    (definv "str.to_int" 0 (in out ctx)
      (declare (integer out))
      (format nil "~s" out))

    (definv "str.from_int" 0 (in out ctx)
      (declare (string out))
      (if (and (every #'digit-char-p out) (not (zerop (length out))))
          (parse-integer out)
          nil))

    (definv "str.indexof" 0 (in out ctx)
      (declare (integer out))
      (make-instance 'duet-refinement
                     :name 'str.indexof-length
                     :refinement-function #'(lambda (input)
                                              (and (<= out (length input))
                                                   (>= out -1)))))
      
    ;; We're deviating a bit here...also don't handle when out == -1
    (definv "str.indexof" 1 (in out ctx)
      (let ((src (nth 0 ctx)))
        (declare (string src) (integer out))
        '(format *trace-output* "; io_inv: ~s ~s --> " src out)
        (cond
          ((= -1 out)
           '(format *trace-output* ":top~%")
           :top)

          ((< out 0)
           '(format *trace-output* "nil~%")
           nil)
          
          ((< (length src) out)
           '(format *trace-output* "nil~%")
           nil)

          (t
           (let ((candidates
                   (loop for ix from out to (length src)
                         collecting (str:substring out ix src))))
             (if (null candidates)
                 (progn
                   '(format *trace-output* "nil~%")
                   nil)
                 (progn
                   '(format *trace-output* "~s~%" candidates)
                   (make-instance 'duet-refinement
                                  :name 'str.indexof-1
                                  :refinement-function
                                  #'(lambda (input)
                                      (member input
                                              candidates
                                              :test #'string=))))))))))

    (definv "str.indexof" 2 (in out ctx)
      (let ((src (nth 0 ctx))
            (sub (nth 1 ctx)))
        (declare (string src sub) (integer out))
        '(format *trace-output* "; io_in2: ~s ~s ~s --> " src sub out)
        (cond
          ((string= sub "")
           '(format *trace-output* "~s~%" out)
           out)

          (t
           (if (and (<= 0 out) (< out (length src)) (search sub src :start2 out))
               (let ((res
                       (loop for ix from out downto 0
                             if (= (search sub src :start2 ix) out)
                               collecting ix)))
                 '(format *trace-output* "~s~%" res)
                 (make-instance 'duet-refinement
                                :name 'str.indexof-2
                                :refinement-function #'(lambda (in)
                                                         (member in res))))
               (progn
                 '(format *trace-output* "nil~%")
                 nil))))))

    (definv "str.replace" 0 (in out ctx &key production)
      (declare (string out))
      (let (witnessed)
        (kl:foreach (comp in (component-library:get-components
                              *component-library*
                              (first (g:occurrences production))))
          (push (get-src (smt:get-first-value
                          (ast:execute-program tdp:*semantics* comp in))
                         out)
                witnessed))
                                        ;(remove-if #'null witnessed)))
        ;(format *trace-output* ";R0: ~s~%" witnessed)
        witnessed))

    (definv "str.replace" 1 (in out ctx)
      (let ((src (nth 0 ctx)))
        (declare (string src out))
        (first
         (remove-if #'null
                    (map 'list #'car (get-rep-pairs src out))))))

    (definv "str.replace" 2 (in out ctx)
      (let ((src (nth 0 ctx))
            (rep (nth 1 ctx)))
        (declare (string src rep out))
        (remove-if #'null
                   (map 'list #'cdr
                        (remove-if-not #'(lambda (x)
                                           (string= (car x) rep))
                                       (get-rep-pairs src out))))))
    
    (definv "ite" 0 (in out ctx)
      nil)))
