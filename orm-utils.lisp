;;;; orm-utils.lisp

(in-package :orm-utils)

;;(ql:quickload "cm2")

(proclaim '(optimize (speed 0) (debug 3)))

;;; (defun filter (fn list)
;;;  (reverse (reduce (lambda (l v) (if (funcall fn v) (cons v l) l)) list :initial-value '())))

(defun reducing (fn seq &key key from-end (start 0) end (initial-value nil ivp))
  (reverse
   (reduce
    (lambda (x y)
      (cons (funcall fn y (first x)) x))
    (rest seq)
    :key key
    :from-end from-end
    :start start
    :end end
    :initial-value (if ivp (list (funcall fn (first seq) initial-value))
                       (list (first seq))))))

(defmacro while (test &body body)
  (let ((gtop (gensym))
        (gend (gensym)))
    `(tagbody
       ,gtop
       (if (not ,test)
           (go ,gend))
       ,@body
       (go ,gtop)
       ,gend)))

(defun filter (pred seq)
  "return a list of all elements of seq satisfying pred."
  (remove-if-not pred seq))

(defmacro ensure-prop (args prop val)
  `(unless (getf ,args ,prop) (setf (getf ,args ,prop) ,val)))

(defun every-nth (l n)
  (loop
     for x in l
     for idx from 0
     if (zerop (mod idx n)) collect x))

;;; (every-nth '(1 2 3 4 5 6 7 8 9 10 11) 3)

(defun str-concat (&rest args)
  (apply #'concatenate 'string args))

(defmacro repeat-format (stream expr num)
  `(format ,stream "~v@{~A~:*~}" ,num ,expr))

#|
(defun filter (pred l)
  "return a list of all elements of l satisfying pred."
  (loop
     for x in l
     if (funcall pred x) collect x))

;;; idiomatic lisp:
(defun filter (fn seq)
  (mapcan (lambda (x) (and (funcall fn x) (list x))) seq))

(defun filter (fn seq)
  (remove-if-not fn seq))

|#


(defun firstn (seq n)
  "return first n elems of seq"
  (subseq seq 0 n))

(defun sort-by (list &key (test #'<) (key #'identity))
  (sort list #'(lambda (x y) (funcall test (funcall key x) (funcall key y)))))

(defun rfind (item tree &key (test #'eql))
  (if (atom tree)
      (if (funcall test item tree) tree)
      (or (rfind item (first tree) :test test)
          (rfind item (rest tree) :test test))))

(defmacro default (expr default)
  "(if expr expr default) without calculating expr twice."
  (let ((myvar (gensym)))
    `(let ((,myvar ,expr))
       (if ,myvar ,myvar ,default))))

(defmacro with-output-to-file ((stream fname &key (if-exists :supersede)) &rest body)
  `(with-open-file (,stream ,fname :direction :output :if-exists ,if-exists)
     ,@body))

(defmacro with-file-stream ((stream fname &key (direction :output) (if-exists :supersede)) &rest body)
  `(with-open-file (,stream ,fname :direction ,direction :if-exists ,if-exists)
     ,@body))

(defmacro with-stream-to-string ((stream string) &rest body)
  `(let ((,string (make-array '(0) :adjustable t :fill-pointer 0 :element-type 'character)))
    (with-output-to-string (,stream ,string)
      ,@body)))

(defun make-adjustable-string ()
  (make-array '(0) :element-type 'character
              :fill-pointer 0 :adjustable t))

(defun last-n (n)
  "returns a function which will retrieve last n elements of a given
list when applied."
        (lambda (list)
          (let ((len (length list)))
            (if (< len n)
                (error (format nil "(last-n ~d) called with too short list: ~a" n list))
                (nthcdr (- (length list) n) list)))))

;;; (funcall (last-n 5) '(1 2 3 4 5 6 7 8)) -> (4 5 6 7 8)



(defun dround (num &optional (prec 2))
  (let ((ept (expt 10 prec)))
    (/ (round (* num ept)) ept 1.0)))

#|
(defun map-tree (fn tree)
  (cond
    ((null tree) nil)
    ((consp (first tree))
     (cons (map-tree fn (first tree))
           (map-tree fn (rest tree))))
    (t (cons (funcall fn (first tree))
             (map-tree fn (rest tree))))))
|#

(defun map-tree (fn tree &key (test (lambda (elem) (not (consp elem)))))
  "map function recursively on all leaf nodes of given
tree (represented as a nested list). leaf nodes are determind by
applying #'test on the list containing them. If the call doesn't
return a cons cell, it is a leaf node. Return the modified tree."
  (cond
    ((null tree) nil)
    ((funcall test (first tree))
     (cons (funcall fn (first tree))
           (map-tree fn (rest tree) :test test)))
    ((consp (first tree)) 
     (cons (map-tree fn (first tree) :test test)
           (map-tree fn (rest tree) :test test)))
    (:else (cons (first tree) (map-tree fn (rest tree) :test test)))))

(defmacro map-params (syms values)
  `(mapcar #'(lambda (sym val) (setf (symbol-value sym) val)) ,syms ,values))

(defmacro def-params (&rest syms)
  `(mapcar #'(lambda (sym) (eval `(defparameter ,sym nil))) ',syms))

;;; Norvigs definition of mappend:

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))


(defun replace-stars-in-string (formstring string)
  (let ((result ""))
    (loop
       for i from 0 below (length formstring)
       do
       (setf result 
             (concatenate 'string result 
                          (if (eq (char formstring i) #\*) 
                              string 
                              (string (char formstring i))))))
    result))


(defun get-time (secs)
  (multiple-value-bind (hr mins)
      (floor secs 3600)
      (multiple-value-bind (min secs)
          (floor mins 60)
        (list hr min secs))))

(defun calcsndbytes (hr min sec &optional (samplerate 44100))
  "calc num of bytes (not samples!) from hr min and sec."
  (let ((ttime (+ (* 3600 hr) (* 60 min) sec)))
    (floor (* ttime samplerate 2))))

;;; (apply #'calcsndbytes (get-time 120.3))
;;; (calcsndbytes 0 0 120.3)

(defun format-time (hr min sec)
  (format nil "~2,'0d:~2,'0d:~:[0~,3f~;~,3f~]" hr min (>= sec 10) sec))

;;; (apply #'format-time (get-time 728.3))

(defun mlist (list count)
  "divide list into sublists of length count. Last element is padded
with nil if list-length is not a multiple of count."
  (loop for i from 0 below (length list) by count
     collect (loop for n from 0 below count collect (elt list (+ n i)))))

;;; (mlist '(1 2 3 4 5) 2) -> ((1 2) (3 4) (5 NIL))

#|

(defun integrate (seq)
  (reducing #'+ seq))


(integrate '(0 1 2 3 4))
|#

(defun integrate (seq &key (modifier #'+) (start (first seq)))
  "return a running sum (or any other modifier fn) of a seq."
  (loop
     for i in seq
     for j = start then (funcall modifier j i)
     collect j))


;; (defun differentiate (liste)
;;   (cons (car liste)
;;         (loop 
;;            for i in liste
;;            for j in (cdr liste)
;;            collect (- j i))))

(defun differentiate (seq &key (modifier #'-) (start (first seq)))
  "return differences between subsequent elements of seq."
  (cons start
	(mapcar (lambda (x y) (funcall modifier x y)) (cdr seq) seq)))

#|
 (differentiate '(1 3 2 5 6 4))
 (differentiate '((1 3) (2 -1) (-2 2) (4 -5) (6 -2) (1 4)) 
 	       :modifier #'(lambda (curr last) (list (first curr) (- (second curr) (second last)))))

 (let ((seq '((:time 0 :keynum 60) (:time 4 :keynum 62) (:time 6 :keynum 65))))
   (differentiate seq
    :modifier (lambda (x y) (append (list :dtime (- (second x) (second y)))
                               (cddr x)))
    :start (cons :dtime (cdr (first seq)))))

|#

;;; remove-brackets in classic style:

(defun remove-brackets (liste)
  (cond ((null liste) '())
        ((consp (car liste)) (append (remove-brackets (car liste))
                                     (remove-brackets (cdr liste))))
        (t (cons (car liste) (remove-brackets (cdr liste))))))

;;; (remove-brackets '(1 2 (a b (c) d (e ((f) 5 6)))))

;;; the same procedure in loop style (in a way better readable):
#|
(defun flatten (list)
  "remove all brackets except the outmost in list."
  (loop for i in list
     append (if (consp i) (flatten i) (list i))))
|#

;;; kept for backwards compatibility:

(declaim (inline not-consp))
(defun not-consp (elem)
  (not (consp elem)))

(defun flatten-fn (seq &key (test #'atom) (key #'identity))
;;;  (break "~a" seq)
  "remove all brackets except the outmost in seq. Use test and key to
   determine where to stop removing brackets.

   Example:

   (flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)))
   -> (a b c d e f g h i k)

   keep one level of brackets:

   (flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)) :key #'car)
   -> ((a b) (c d) (e f) (g h) (i k))
   "
  (cond ((null seq) nil)
        ((funcall test (funcall key seq)) seq)
        ((funcall test (funcall key (first seq)))
         (cons (first seq)
               (flatten-fn (rest seq) :test test :key key)))
        ((consp (first seq))
         (append (flatten-fn (first seq) :test test :key key)
                 (flatten-fn (rest seq) :test test :key key)))
        (t (append (flatten-fn (first seq) :test test :key key)
                   (flatten-fn (rest seq) :test test :key key)))))



#|
 (flatten-fn '(1 2 3 (a b)(((c d) (e f)) (g h)) (i k)))
 -> (a b c d e f g h i k)

 (flatten-fn '((a b)(((c d) (e f)) (g h)) (i k)) :fn #'caar)
 -> ((a b) (c d) (e f) (g h) (i k))

|#

(defun flatten (obj)
  "non-recursive, non-stack version from Rosetta Code."
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

;;; calc fibonacci number directly:

(defun fibonacci (n)
  (declare (integer n))
  (let ((sr5 (sqrt 5)))
    (round (* (/ 1 sr5) 
              (- (expt (/ (+ sr5 1) 2) n) 
                 (expt (/ (- 1 sr5) 2) n))))))

;; (loop for n from 0 to 16 collect (calcfibo n))

;;; classic recursive iterative definition:

(defun fib (n)
  (declare (integer n))
  (labels ((fib-iter (i last-1 last)
             (declare (integer i))
             (if (= i 0) last
                 (fib-iter (- i 1) last (+ last-1 last)))))
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (t (fib-iter (- n 1) 0 1)))))

;;; (time (loop for n from 0 to 1600 do (fib n)))
;;; (time (loop for n from 0 to 1600 do (calcfibo n))) -> floating-point overflow (!!)

;; returns a function of n which exponentially interpolates between
;; min and max in n steps satisfying f(0) = min and f(n-1) = max

(defun ip-exp (min max &optional (steps 2))
  (let ((st (- steps 1))
        (base (/ max min)))
    (lambda (n)
      (* min (expt base (/ n st))))))

;; usage: (defparameter cl1 nil)
;; (setf cl1 (ip-exp 0.01 1000 10))
;; (defun cl1 (n) (funcall (ip-exp 0.01 1000 10) n))
;; (loop for x from 0 below 10 collect (funcall cl1 x))
;;
;; without third arg function is normalized:
;;
;; (setf cl1 (ip-exp 0.01 1000))
;; (loop for x from 0 to 1  by 1/10 collect (funcall cl1 x))

;; returns a function of n which logarithmically interpolates between
;; min and max in n steps satisfying f(0) = min and f(n-1) = max

(defun ip-log (min max MMIN MMAX)
  (let ((diff (- MMAX MMIN))
        (denom (log (/ max min))))
    (lambda (n)
      (+ MMIN (* diff (/ (log (/ n min)) denom))))))

;; usage: (defparameter cl1 nil)
;; (setf cl1 (ip-log 1 10 1000 10000))
;; or alternatively: (defun cl1 (n) (funcall (ip-log 0.01 1000 10) n))
;; (loop for x from 1 below 11 collect (funcall cl1 x))

(defun ip-lin (min max &optional (steps 2))
  (let ((slope (/ (- max min) (- steps 1))))
    (lambda (n)
      (+ min (* slope n)))))

;; usage: (defparameter cl1 nil)
;; (setf cl1 (ip-lin 0 1000 10))
;; (defun cl1 (n) (funcall (ip-lin 0.01 1000 10) n))
;; (loop for x from 0 below 10 collect (funcall cl1 x))

(defun fv->ct (fv)
  (* 1200 (log fv 2)))

(defun ct->fv (ct)
  (expt 2 (/ ct 1200)))

(defun mtof (m &key (tuning-base 440))
  (* tuning-base (expt 2 (/ (- m 69) 12))))

;;; (mtof 81) -> 880

(defun ftom (f &key (tuning-base 440))
  (+ 69 (* 12 (log (/ f tuning-base) 2))))

;;; (ftom 880) -> 81.0

;; helper function for rotate: It destructively modifies its argument
;; to an endless list and returns the size of the original list.

(defun nconc-get-size (list)
  (labels ((inner-loop (x n head)
             (if (null (cdr x))
                 (progn
                   (setf (cdr x) head)
                   (incf n))
                 (inner-loop (cdr x) (incf n) head))))
    (inner-loop list 0 list)))

;; (defparameter test '(1 2 3))
;; (nconc-get-size test)
;; (loop for x in test for z from 1 to 5 collect x)

;; new rotate function which only traverses the list one time,
;; determining the length and nconcing it in the same step. It uses
;; nconc-get-size as helper function. Function ist non-destructive: It
;; uses a local copy of list instead of the original.

(defun rotate (list &optional (num 1))
  "rotate a list by n elems (to the right). n can be negative. If n is
larger than the list size it will wrap around as if the rotation was
called recursively n times."
  (let* ((new-list (copy-tree list))
         (num-normalized (mod (* -1 num) (nconc-get-size new-list)))) ;; innermost function nconcs new-list to an endless list and gets size in one step
    (if (zerop num-normalized) list
      (let* ((newlast (nthcdr (- num-normalized 1) new-list)) ;; determine the new last element of the list (wraps around for nums > (length list) or < 0) and store into variable
             (newfirst (cdr newlast))) ;; the new listhead is the cdr of the last element; also stored in variable
        (setf (cdr newlast) nil) ;; set the cdr of the new last element to nil
        (values newfirst))))) ;; return the new first element (listhead)

(defun transpose-list (list-of-lists)
  (apply #'mapcar #'list list-of-lists))
;; (rotate '(0 1 2 3 4 5) -3) -> (3 4 5 0 1 2)
;;
;; (defparameter test '(0 1 2 3 4 5))
;;
;; (rotate test 3)
;; test

;;; group function from Graham's book:

(defun group (source n)
  "group elems of list into sublists of length n"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun group-by (source g-seq)
  "group elems of list into sublists of lengths given by g-seq cyclically"
  (let* ((tmp (copy-list g-seq ))
         (gseq (nconc tmp tmp)))

    (labels ((rec (source acc)
               (let* ((n (first gseq))
                      (rest (nthcdr n source)))
                 (if (zerop n) (error "zero length"))
                 (setf gseq (rest gseq))
                 (if (consp rest)
                     (progn
                       (rec rest (cons (subseq source 0 n) acc)))
                     (nreverse (cons source acc))))))
      (if source (rec source nil) nil))))

;;; (group-by '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6) '(2 3 5))
;;; => ((1 2) (3 4 5) (6 7 8 9 1) (2 3) (4 5 6) (7 8 9 1 2) (3 4) (5 6))

(defun group-by-key (source &key (test #'=) (key #'car))
  "group elems of source into sublists depending on test and key. Source has
to be sorted according to test!"
  (let ((res nil))
    (labels ((rec (source last acc)
               (let ((k (funcall key (car source))))
                 (cond
                   ((null source) (if acc (push (reverse acc) res)))
                   ((funcall test last k)
                    (rec (cdr source) last (push (car source) acc)))
                   (:else (progn
                            (push (reverse acc) res)
                            (rec (cdr source) k (list (car source)))))))))
      (if source (reverse (rec source (funcall key (first source)) nil)) nil))))

;;; (group-by-key '((0 1) (0 3) (1 4) (2 5)))

;;; Exercise of sicp:

(defun repeated (n f)
  ;; repeated returns a function which applies a given function f n
  ;; times onto itself. f must be a function of at least one argument
  ;; which returns one argument. The return value of the function
  ;; replaces the first argument in the next recursive call, leaving
  ;; all other arguments as they were.
  (cond ((= n 1)
         (lambda (&rest args) (apply f args)))
        (t (lambda (&rest args)
             (apply f (cons (apply (repeated (- n 1) f) args) (rest args)))))))

;;; Example:

;;; (funcall (repeated 4 (lambda (x y) (* x y))) 1 2) ; -> 16

(defun permute (list permutation)
  (loop 
     for x in permutation
     collect (nth x list)))

;; (apply (repeated 2 #'permute)
;;        '((0 1 2 3 4)
;;          (2 0 4 1 3)))

;; or, alternatively:

;; (funcall (repeated 2 #'permute)
;;          '(0 1 2 3 4)
;;          '(2 0 4 1 3))


;;; getting rid of the funcall:

(defun do-repeated (n func &rest args)
  (apply (repeated n func)
         args))

;; (do-repeated 2 #'permute '(A B C D E) '(2 0 4 1 3))

;;; The disadvantage of the last form is that repeated gets evaluated
;;; (compiled) each time, the do-repeated is called. To capture the
;;; function for multiple applications, the following method can be
;;; used. Note that we can use myfunc with any arguments, which is one
;;; of the advantages of abstracting the function application!


;; (let ((myfunc (repeated 3 #'permute)))
;;   (list
;;    (funcall myfunc '(A B C D E F) '(2 5 0 4 1 3))
;;    (funcall myfunc '(0 1 2 3) '(2 0 3 1))
;;    (funcall myfunc '(0 1 2 3 4) '(2 0 3 4 1))))


;; (setf *test* (make-cyclic-pattern '(0 1 2 3 4 5) 2))
;; (setf *test* (make-random-pattern '(0 1 2 3 4 5)))

;; (gnext *test* 100)

;; (defparameter pat1 nil)
;; (setf pat1 (new-permutation :of '(A B C D E) :permutation '(3 4 1 2 0)))
;; (next pat1 #t) -> (A B C D E)
;; (next pat1 #t) -> (D E B C A)
;; (next pat1 #t) -> (C A E B D)
;; (next pat1 #t) -> (B D A E C)
;; (next pat1 #t) -> (E C D A B)
;; (next pat1 #t) -> (A B C D E)

(defun n-apply (n fn &key (initial-value '()) (collect nil))
  "call fn n times accumulating the results in acc. fn should accept two
values, the current n and the accumulated results of previous
calls. If collect is t return all results in a list."
  (labels ((inner (idx acc)
             (if (< idx n)
                 (inner (1+ idx) (cons (funcall fn idx acc) acc))
                 (if collect (reverse acc) (first acc)))))
    (inner 0 initial-value)))


(defun secs->timestr (secs &key (format 0))
  (ecase format
    (0
     (format nil "~2,'0d:~5,2,0,'#,'0f" 
             (floor secs 60) 
             (mod secs 60.0)))
    (1
     (format nil "~2,'0d:~2,'0d:~5,2,0,'#,'0f"  
             (floor (floor secs 60) 60) 
             (mod (floor secs 60) 60) 
             (mod secs 60.0)))
    (2
     (format nil "~2,'0d:~2,'0d:~2,'0d"  
             (floor (floor secs 60) 60) 
             (mod (floor secs 60) 60) 
             (floor (mod secs 60))))))

;;; (secs->timestr   72.5 :format 0) ->    "01:12.50"
;;; (secs->timestr 3672.5 :format 1) -> "01:01:12.50"
;;; (secs->timestr 3672.5 :format 2) -> "01:01:12"

(defun secs->beats (secs &key (tempo '(1/4 60)) (unit 1/4))
  "return number of beats for a given time in seconds. keys are :tempo (default '(1/4 60)) and a unit to count (default 1/4)"
  (let ((time-per-unit (* unit (/ 60 (apply #'* tempo)))))
    (/ secs time-per-unit)))

;; (secs->beats 2 :tempo '(1/4 120) :unit 1/4)


(defun beats->secs (beats &key (tempo '(1/4 60)) (unit 1/4))
"return the time in secs of given number of beats. Keys are :tempo (default '(1/4 60)) and :unit of the beats (default 1/4)"
  (let ((time-per-unit (* unit (/ 60 (apply #'* tempo)))))
    (* time-per-unit beats)))

;; (beats->secs 20 :tempo '(1/4 120) :unit 1/4)


;;; das sollte alles nicht hier stehen!!

(defun extract-events (eventlist &key (startpos 0) (endpos nil))
 (let* (
        (local-eventlist (copy-tree eventlist))
        (started nil)
        (end-p nil)
        (start-offs 0))
   (loop
    for x in local-eventlist
    until end-p
    append
    (let ((curr-event-time (nth 1 x)))
      (progn 
;        (format t "~a~%" curr-event-time)
        (if
            (AND
             (OR started (>= curr-event-time startpos))
             (<= curr-event-time endpos))
          (progn
            (when (NOT started) (progn
                                  (setf started T)
                                  (setf start-offs curr-event-time)))
            (decf (nth 1 x) start-offs)
            (list x))
          (progn
            (when
                (> curr-event-time endpos)
              (setf end-p 'T))
            nil)))
      ))))

(defparameter *timesiglist* nil)

(defun find-abs-pos (bar-pos &OPTIONAL (timesiglist *timesiglist*))
  (let ((local-timesiglist (copy-tree timesiglist))
        (curr 0))
   (loop
    for bar-count from 0 below (car bar-pos)
    do
;    (format t "~a " bar-count)
    (incf curr (apply #'/ (pop local-timesiglist))))
   (incf curr (cadr bar-pos))
   curr)
  )

(defparameter *tempo* nil)
(setf *tempo* '(1/4 60))

(defun amp->db (amp)
  (if (zerop amp) -100
      (* 20 (log amp 10))))

;;; (amp->db 1) -> 0
;;; (amp->db 0.5) -> -6
;;; (amp->db 2) -> 6

(defun db->amp (db)
  (expt 10 (/ db 20)))

;;; (db->amp 0) -> 1
;;; (db->amp -6) -> 0.5
;;; (db->amp 6) -> 1.9952623
;;; (db->amp -60) -> 1/100

(defun clip (val minvalue maxvalue)
  (min maxvalue (max minvalue val)))

;;; (dolis   q(x (sv evt elems))
;;;   (print x))

;;; (sin (/ pi 8))

;;; (cos (/ pi 8))

(defmacro mapply (fn liste)
  `(mapcar #'(lambda (x) (apply ,fn x)) ,liste))

(defun file-string (infile)
  (with-open-file (stream infile)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun partition-seq (seq pred)
  "generic function to partition a seq into sublists based on a
   predicate called on successive events. Pred is a function of two
   args, an element of the seq and its successor. If pred returns
   non-nil, a new subseq is started after the curren element. The
   result contains all elements of the original seq in orginal order.

   Example: (partition-seq '(1 2 4 5 6 8 9) #'(lambda (x y) (> (- y x) 1))) 
   -> ((1 2) (4 5 6) (8 9))"
(loop 
   with res = () 
   with curr = () 
   for restseq on seq
   do (progn
        (if (and
             (second restseq)
             (funcall pred (first restseq) (second restseq))) 
            (progn
              (push (reverse (push (first restseq) curr)) res)
              (setf curr nil))
            (progn
              (push (first restseq) curr))))
   finally (return (reverse (push (reverse curr) res)))))

;;; (partition-seq '(1 2 4 5 6 8 9) #'(lambda (x y) (> (- y x) 1))) -> ((1 2) (4 5 6) (8 9))

(defmacro with-shadowed-variable ((var) &rest body)
  `(let ((,var ,var))
     ,@body))

(defun make-quantlist (vals)
  "given a list of divisions per beat, return the sorted list of
quantization points in fractions of a beat [0..1]"
  (sort
   (remove-duplicates
    (reduce
     (lambda (acc x) (append acc (loop for n from 1 below x collect (/ n x))))
     vals :initial-value '(0 1)))
   #'<))

(defun quantize-time (val &key (quantlist (make-quantlist '(3 4 5))))
  "quantize the fractional part of val to a quantization list of
possible quantization points in the range [0..1]."
  (multiple-value-bind (int frac) (floor val)
    (loop for (last curr) on quantlist until (<= last frac curr)
          finally (return (+ int (if (<= (- frac last) (- curr frac)) last curr))))))

;;; (quantize 14.71) -> 59/4 (14.75)


(defun insert (elem result &key (key #'first) (test #'eq))
  "helper function for splice: Inserts an element into a sublist of
result, if one exists with key elements equal to the key of elem,
otherwise it appends a new sublist containing elem at the end of
result.  The function returns the updated result."
  (cond ((null result) (list (list elem)))
        ((funcall test (funcall key elem) (funcall key (caar result)))
         (cons (cons elem (first result)) (rest result)))
        (t (cons (first result) 
                 (insert elem (rest result) :key key :test test)))))

(defun splice (list &key (key #'first) (test #'eq))
  "put the elements of list which contain the same key element into
sublists and return a list of all sublists."
  (let ((result '()))
      (dolist (elem list)
         (setf result (insert elem result :key key :test test)))
     (mapcar #'nreverse result)))

#|
(defun splice (list &key (key #'first) (test #'eq))
  "put the elements of list which contain the same key element into
sublists and return a list of all sublists."
  (loop
     with result = '()
     for elem in list
     do (setf result (insert elem result :key key :test test))
     finally (return (mapcar #'nreverse result))))
|#

#|

(splice '((1 "hallo") (2 "peng") (3 "blah") (1 "naja"))
        :key #'first :test #'=) 

-> (((1 "hallo") (1 "naja")) ((2 "peng")) ((3 "blah")))
|#

(defun get-duplicates (list &key (test #'eql) (once nil))
  "return all duplicate elems of list satisfying the test predicate.
If once is set, return each duplicate element only once (default ist nil)."
  (let ((all-duplicates
          (loop for x on list
             append (if (member (first x) (rest x) :test test) (list (first x))))))
    (if once 
        (remove-duplicates all-duplicates :test test)
        all-duplicates)))

;; (get-duplicates '(1 3 2 4 1 2 5 1) :once t)

(defun all-permutations (seq &key (test #'eql))
  "get all permutations of a sequence. Make sure to supply a :test
function in case the elements can't be compared with #'eql, otherwise
the function will blow the stack!"
  (if (null seq) (list nil)
      (mapcan (lambda (elem)
                (mapcar (lambda (p)
                          (cons elem p))
                        (all-permutations (remove elem seq :test test))))
              seq)))

;;; (all-permutations '("a" "b" "c"))

;;; (time (all-permutations '(1 2 3 4 5 6 7)))

#|

;;; same solution with loop:

(defun all-permutations (seq &key (test #'eql))
  "get all permutations of a sequence. Make sure to supply a :test
function in case the elements can't be compared with #'eql, otherwise
the function will blow the stack!"
  (if (null seq) (list nil)
      (loop for elem in seq
         append (loop for p in (all-permutations
                                (remove elem seq :test test))
                 collect (cons elem p)))))
|#

#|
;;; obsolete as #'combinations can handle this.

(defun all-pairs (seq)
  "get all pairs of a sequence."
  (if (null seq) (list nil)
      (loop for (first . tail) on seq
         nconc (mapcar (lambda (second) (list first second)) tail))))

(all-pairs '(1 2 3 4))
|#

(defun combinations (seq &optional (n 2))
  "get all n combinations of seq."
  (cond
    ((zerop n) '(()))              ; one combination of zero elements.
    ((null seq) '())               ; no combination from no element.
    (t (nconc (mapcar (lambda (combi) (cons (first seq) combi))
                      (combinations (rest seq) (1- n)))
              (combinations (rest seq) n)))))

;;; (combinations '(1 2 3 4 5) 2)

(defun reverse-all (list)
  "recursively reverse list."
  (let ((result nil))
    (dolist (element list result)
      (push
       (if (listp element) (reverse-all element) element)
       result))))

#|
(defun range (a1 &optional (a2 nil))
  "like clojure's range (except it's not lazy!)"
  (if a2
      (loop for x from 1 to a2 collect x)
      (loop for x below a1 collect x)))

;; (range 1 10)
|#

(defun get-exp-fn (minspeed maxspeed startpos endpos)
  (let* ((b (/ minspeed (- maxspeed minspeed)))
	 (a (/ (+ b 1) b)))
    (lambda (x)
      (+ startpos (* (- endpos startpos) (- (* b (expt a x)) b))))))

(defun get-abs-dur (minspeed maxspeed startpos endpos)
  (/ (* (log (/ maxspeed minspeed)) 
	(- endpos startpos))
     (- maxspeed minspeed)))

(defun param-exp-func (endpos startdiff enddiff)
  "with given endpos, startdiff and enddiff return an exponential
  function fn with the following property:
   (and
    (= (- (fn 1) (fn 0)) startdiff)
    (= (- (fn endpos) (fn (- endpos 1))) enddiff))
"
  (let* ((startpos 0)
         (fn (get-exp-fn startdiff enddiff startpos endpos))
;;;         (dur (get-abs-dur startspeed endspeed startpos endpos))
         (fac (/ startdiff (* -1 (funcall fn (/ (- 1 endpos)))))))
    (lambda (x) (+ startdiff (* fac (funcall fn (/ (- x 1) (- endpos 1))))))))

#|
;;; example:

(let* ((n 14)
       (startdiff 2)
       (enddiff 0.1)
       (fn (param-exp-func n startdiff enddiff)))
  (loop for x from 0 to n
     collect (funcall fn x)))

;;; -> '(0.0 2.0 3.5883667 4.8498206 5.851647 6.6472807 7.279159 7.780987 8.17953
;;;      8.496046 8.747419 8.947054 9.105601 9.231518 9.331517)

;;; and:

(cdr 
 (ou:differentiate 
  (let* ((n 14)
         (startdiff 2)
         (enddiff 0.1)
         (fn (param-exp-func n startdiff enddiff)))
    (loop for x from 0 to n
       collect (funcall fn x)))))

;;; -> (2.0 1.5883667 1.2614539 1.0018263 0.7956338 0.6318784 0.5018277 0.39854336
;;;     0.31651592 0.2513733 0.19963455 0.1585474 0.12591648 0.09999943)

;;; this is the same as:

(let* ((n 14)
       (startdiff 2)
       (enddiff 0.1))
  (loop for x below n collect (* startdiff (expt (/ enddiff startdiff) (/ x (- n 1))))))

;;; -> (2.0 1.5883666 1.2614543 1.001826 0.79563355 0.6318789 0.50182766 0.39854318
;;;     0.31651637 0.251372 0.19963546 0.15854716 0.12591551 0.1)

|#

(defun call/collecting (f n tail)
  (let ((c (make-collector)))
    (dotimes (i n (collector-contents c tail))
      (collect-into c (funcall f i)))))

(defmacro v-collect ((v n &optional (tail '())) form)
  "return a list of n elems prepended to tail by evaluating form n times
with the symbol n bound to the iteration index in the lexical scope of
form.

Example:

(v-collect (n 10) (* n n)) ;-> (0 1 4 9 16 25 36 49 64 81)
"
  `(call/collecting (lambda (,v) 
                      (declare (ignorable ,v))
                      ,form)
                    ,n ,tail))

;;; (v-collect (n 10) (* n n)) ;-> (0 1 4 9 16 25 36 49 64 81)

#|
;;; experimental, seems to be less efficient than v-collect.

(defmacro v2-collect ((v n &optional (tail '())) form)
  (let ((my-fn (gensym "fn"))
        (idx (gensym "idx"))
        (seq (gensym "seq"))
        (num (gensym "num")))
    `(let ((,num ,n))
       (labels
           ((,my-fn (,idx ,seq)
              (if (>= ,idx ,num)
                  ,seq
                  (,my-fn
                   (+ ,idx 1)
                   (cons (let ((,v ,idx))
                           (declare (ignorable ,v))
                           ,form)
                         ,seq)))))
         (,my-fn 0 ,tail)))))

(defmacro n-collect (n form &key (initial-element '()))
  "return a list of n elems prepended to initial-element by
evaluating form n times with the symbol n bound to the iteration
index in the lexical scope of form."
  (let ((n-var (intern "N")))
    `(labels
         ((fn (idx seq)
            (if (< idx ,n)
                (cons
                 (let ((,n-var idx))
                   (declare (ignorable ,n-var))
                   ,form)
                 (fn (+ idx 1) seq))
                seq)))
       (fn 0 ,initial-element))))

;;; (n-collect 11 n)
|#

(defun repeat (n elem)
  "return a list with n occurences of elem.
Example:

(repeat 10 5) ;-> (5 5 5 5 5 5 5 5 5 5)
"
  (v-collect (v n) elem))

;;; (repeat 10 5)

(defun range (&rest args)
  "like clojure's range.
Arities:
(range end) 
(range start end) 
(range start end step) 

Return a list of nums from start (inclusive) to end (exclusive) by
step. Start and step are optional args defaulting to 0 and 1
respectively.

Example:

(range 1 10 2) ;-> (1 3 5 7 9)
"
  (destructuring-bind (start end step)
      (cond
        ((third args) args)
        ((second args) (list (first args) (second args) 1))
        (t (list 0 (first args) 1)))
    (loop for i from start below end by step collect i)))

#|

;;; recursive definition

(defun range (&rest args)
  (destructuring-bind (start end step)
      (cond
        ((third args) args)
        ((second args) (list (first args) (second args) 1))
        (t (list 0 (first args) 1)))
    (labels ((inner (i) (if (< i end) (cons i (inner (+ i step))))))
      (inner start))))


|#

;;; (range 0 10 1)
(defun sum-x (n)
 (* n (+ n 1) 1/2))

#|
(defun map-indexed (result-type fn &rest seqs)
  "map fn over seqs with incrementing idx. The idx will get supplied
as first arg to fn and is reset for each seq."
  (map result-type
       (lambda (seq)
         (let ((idx -1))
           (map result-type (lambda (elem) (funcall fn (incf idx) elem)) seq)))
       seqs))
|#

(defmacro map-indexed (result-type fn &rest seqs)
  "map fn over seqs with incrementing idx. The idx will get supplied
as first arg to fn and is reset for each seq."
  (let ((i (gensym)))
    `(let ((,i -1))
       (map ,result-type
        (lambda (&rest args)
          (apply ,fn (incf ,i) args))
        ,@seqs))))


(defmacro case-ext (keyform test &rest body)
  "case with compare function as second element."
  `(cond
     ,@(mapcar (lambda (clause)
                 (list (list 'funcall test keyform (first clause)) (second clause)))
               body)))

#|
(defmacro f (args &rest body)
  `(lambda ,args ,@body))
|#

(defmacro let-default (((sym test default)) &body body)
  (let ((a (gensym)))
    `(let* ((,a ,test)
            (,sym (if ,a ,a ,default)))
       ,@body)))

(defmacro setf-default (sym test default)
  (let ((h (gensym)))
    `(let ((,h ,test))
       (setf ,sym (if ,h ,h ,default)))))

(defun spit (seq &key (outfile "/tmp/test.lisp"))
  "print seq to outfile, each element on a new line."
  (with-open-file (out outfile
                       :direction :output
                       :if-exists :supersede)
    (dolist (line seq)
      (format out "~A~%" line))))


(defun slurp (file)
  "return contents of file as a list of all lines read individually by
the lisp reader."
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
       collect (read-from-string line))))

(defun slurp-string (file)
  "return contents of file as a string."
  (with-open-file (stream file)
    (with-output-to-string (str)
      (loop for line = (read-line stream nil)
         while line
         do (format str "~a~%" line)))))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun map-all-pairs (fn seq)
  "Execute fn on all possible pairs of two different elements of
seq. The pairs are given to fn in the order of appearance in the seq."
  (loop
     for (b1 . rest) on seq
     do (loop for b2 in rest do (funcall fn b1 b2))))

(defmacro with-curr-dir ((dir) &body body)
  "set the cwd to dir in the body, return the result of body after
resetting the cwd."
  (let ((cwd (gensym "cwd"))
        (res (gensym "res")))
    `(let ((,cwd ,(sb-posix:getcwd))
           ,res)
       (sb-posix:chdir ,dir)
       (setf ,res (progn ,@body))
       (sb-posix:chdir ,cwd)
       ,res)))

(defun date-string ()
  "return a string of the current time formatted
  \"yyyy-mm-dd-hr-min-sec\""
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d"
	    year
	    month
	    date
	    hour
	    minute
	    second)))

(defmacro push-if (form list)
  "push form to list if form evaluates to non-nil."
  (let ((result (gensym "result")))
    `(let ((,result ,form))
       (if ,result (push ,result ,list)))))

(defparameter *last-random-state* nil)

(defun memorize-random-state ()
  (setf *last-random-state* (make-random-state *random-state*)))

(defun recall-random-state ()
  (setf *random-state* *last-random-state*))

(defun count-elements-generic-test (list &key (test #'eql))
  "count the number of occurences of all different elems in
list. Return the results as list with sublists of the form (elem
count) for each elem. :test can be any test function of 2 args."
  (loop with all-numbers = t
     with result = '()
     with found = '()
     for e in list do
     (unless (member e found :test test)
       ;; MDE Thu Jan  3 16:16:30 2013 
       (unless (numberp e) (setf all-numbers nil))
       (push e found)
       (push (list e (count e list :test test)) result))
     finally (return
               (if all-numbers
                   ;; sort by element
                   (sort result #'(lambda (x y) (< (first x) (first y))))
                   ;; sort by number of elements
                   (sort result #'(lambda (x y) (> (second x) (second y))))))))

;;; (count-elements '(1 3 (2 3) 7 (2 3) 2) :test #'equal)
#|
(defun count-elements (list &key (test #'eql) (key #'identity))
  "count the number of occurences of all different elems in list
extracted from the list items according to the :key function. Return
the results as list with sublists of the form (elem count) for each
elem. :test has to be a test function accepted by #'make-hash-table,
:key defaults to #'identity."
  (let ((hash (make-hash-table :test test))
        (result '())
        (all-numbers t))
    (dolist (item list)
      (let ((elem (funcall key item)))
        (unless (gethash elem hash)
          (let ((num (count elem list :test test :key key)))
            (unless (numberp elem) (setf all-numbers nil))
            (setf (gethash elem hash) num)
            (push (list elem num) result)))))
    (if all-numbers
        (sort result #'< :key #'first)
        (sort result #'> :key #'second))))
|#

(defun count-elements (seq &key (test #'eql) (key #'identity) (sort t))
  "count the number of occurences of all mutually different elems in
seq extracted from the list items according to the :key function. 

Return the results as list with sublists of the form (elem count) for
each elem.

:test has to be a test function accepted by #'make-hash-table
:key defaults to #'identity

If :sort is nil the items in the result are in the order of their
first occurence, if :sort is :from-end they are in reverse order of
occurence, if :sort is t they are either sorted by their value if all
elems are numbers or by the number of occurences otherwise.

Works on all sequence types."
  (let* ((hash (make-hash-table :test test))
         (all-numbers t)
         (result '()))
    (map nil
         (lambda (item)
           (let ((elem (funcall key item)))
             (unless (gethash elem hash)
               (let ((num (count elem seq :test test :key key)))
                 (unless (numberp elem) (setf all-numbers nil))
                 (setf (gethash elem hash) num)
                 (push (list elem num) result)))))
         seq)
    (cond ((not sort) (nreverse result))
          ((eq sort :from-end) result)
          (all-numbers (sort result #'< :key #'first))
          (:else (sort result #'> :key #'second)))))



#|


(count-elements '(1 3 2 4 3 2 1 5 4 3 6 5 7 "hallo" 5 "hallo" 6 "peng" 7 4 6 5 7 2 3) :test #'equal)

(count-elements #(1 3 2 4 3 2 1 5 4 3 6 5 7 "hallo" 5 "hallo" 6 "peng" 7 4 6 5 7 2 3) :test #'equal)

(count-elements
 (loop for x below 100 collect (cm:new cm:midi :keynum (cm:between 60 73)))
 :key #'cm::midi-keynum)

(defun parse-props (props seq)
  (loop for prop in props
        collect (list
                 prop
`(getf ,seq ,(intern (string-upcase (symbol-name prop)) 'keyword)))))

(defmacro with-props (props seq &body body)
  `(let ,(parse-props `,props `,seq)
     ,@body))
|#

(defun delete-props (proplist &rest props)
  "destructively remove props from proplist and return it."
  (mapc (lambda (prop) (remf proplist prop)) props)
  proplist)

(defun get-props-list (proplist props &key (force-all nil))
  "create a new proplist by extracting props and their values from
proplist. Props not present in proplist are ignored."
  (reduce (lambda (seq prop) (let ((val (getf proplist prop :not-supplied)))
                          (if (eql val :not-supplied)
                              (if force-all
                                  (list* prop nil seq)
                                  seq)
                              (list* prop val seq))))
          (reverse props)
          :initial-value nil))

(defmacro with-props (vars proplist &body body)
  "like with-slots but using a proplist instead of a class instance."
  `(let ,(mapcar
          (lambda (sym) (list sym `(getf ,proplist ,(intern (symbol-name sym) :keyword))))
          vars)
     ,@body))

;;; (ou:with-props (amp keynum) '(:amp 1 :keynum 60) (list amp keynum)) => (1 60)

(defmacro map-proplist (fn proplist)
  "like mapcar but traversing a property list. fn has to accept two
values, the key and the value of each property in the proplist."
  `(loop for (key value) on ,proplist by #'cddr
         collect (funcall ,fn key value)))

;;; (map-proplist #'list '(:a 2 :b 5 :c 4)) => ((:a 2) (:b 5) (:c 4))

(defmacro do-proplist ((key value) proplist &body body)
  "like dolist but traversing a property list. fn has to accept two
values, the key and the value of each property in the proplist."
  `(loop for (,key ,value) on ,proplist by #'cddr
         do ,@body))

;;; (do-proplist (key val) '("a" 2 "b" 5 "c" 4) (format t "key: ~a, val: ~a~%" key val))

(defmacro with-proplist/collecting ((key value) proplist &body body)
  "like do-props but collecting the result."
  `(loop for (,key ,value) on ,proplist by #'cddr
         collect ,@body))

;;; (with-proplist/collecting (key val) '(:a 2 :b 5 :c 4) (list key (1+ val))) => ((:a 3) (:b 6) (:c 5))

(defun get-prop (proplist key &optional default)
  "like getf but using #'equal for testing."
  (or (second (member key proplist :test #'equal)) default))


(defun n-lin (x min max)
  "linear interpolation for normalized x."
  (float (+ min (* (- max min) x))))

;;; (n-lin 0 10 1000) -> 10
;;; (n-lin 0.5 10 1000) -> 505.0
;;; (n-lin 1 10 1000) -> 1000

(defun lin-n (val min max)
  "return normalized val linearly between min and max.

(lin-n min min max) -> 0
(lin-n max min max) -> 1
"
  (/ (- val min) (- max min)) 1.0)

(defun n-exp (x min max)
  "linear interpolation for normalized x."
  (float (* min (expt (/ max min) x))))

;;; (n-exp 0 10 1000) -> 10
;;; (n-exp 0.5 10 1000) -> 100.0
;;; (n-exp 1 10 1000) -> 1000

(defmacro exp-n (val min max)
  "return normalized val exponentially between min and max:

(exp-n min min max) -> 0
(exp-n max min max) -> 1
"
  (let ((quot (if (zerop min) 0 (/ max min))))
    `(log ,(/ val min) ,quot)))

(defun m-exp (x min max)
  "exp interpolation for midivalues (x = [0..127])"
  (n-exp (/ x 127) min max))

(defun n-exp-zero (x min max)
  "exp interpolation for normalized values (x = [0..1]) with 0 for x = 0"
  (float
   (if (zerop x) 0
       (* min (expt (/ max min) x)))))

(defun m-exp-zero (x min max)
  "exp interpolation for midivalues (x = [0..127]) with 0 for x = 0"
  (n-exp-zero (/ x 127) min max))

;;; (n-exp 0 10 1000) -> 10
;;; (n-exp 0.5 10 1000) -> 100.0
;;; (n-exp 1 10 1000) -> 1000

(defun m-lin (x min max)
  "linear interpolation for midivalues (x = [0..127])"
  (+ min (* (- max min) (/ x 127))))

(defun ntom (n)
  (round (* n 127)))

(defun mton (m)
  (float (/ m 127)))

;;; (n-lin 0 10 1000) -> 10
;;; (n-lin 0.5 10 1000) -> 505.0
;;; (n-lin 1 10 1000) -> 1000

(defun mcn-lin (x min max)
  "linear interpolation for midivalues (x = [0..127])"
  (n-lin (/ x 127) min max))

(defun mcn-exp (x min max)
  "exponential interpolation for midivalues (x = [0..127])"
  (n-exp (/ x 127) min max))

(defun r-exp (min max)
  "random value between [min..max] with exponential distribution."
  (* min (expt (/ max min) (random 1.0))))

(defun r-lin (min max)
  "random value between [min..max] with linear distribution."
  (+ min (* (- max min) (random 1.0))))

(defun randm (max)
  "random value between [min..max] with linear distribution."
  (r-lin 0 max))

(defun rand (max)
  "random value between [0..max-1] with linear distribution."
  (r-lin 0 max))

(defun n-exp-dev (x max)
  "return a random deviation factor, the deviation being exponentially
interpolated between 1 for x=0 and [1/max..max] for x=1."
  (r-exp
   (n-lin x 1 (/ max))
   (n-lin x 1 max)))

(defun r-exp-dev (max)
  "return a random deviation factor, the deviation being exponentially
interpolated between 1 for x=0 and [1/max..max] for x=1."
  (n-exp-dev (random 1.0) max))

(defun n-lin-dev (x max)
  "return a random deviation offset, the deviation being linearly
interpolated between 0 for x=0 and [-max..max] for x=1."
  (if (zerop x)
      0
      (* max (- x (random (* 2.0 x))))))

(defun n-lin-fn (min max)
  "linear interpolation for normalized x."
  (lambda (x) (n-lin x min max)))

(defun n-exp-fn (min max)
  "exponential interpolation for normalized x."
  (lambda (x) (n-exp x min max)))

(defun n-lin-rev-fn (min max)
  "reverse of linear interpolation for normalized x."
  (lambda (x) (/ (- x min) (- max min))))

(defun n-exp-rev-fn (min max)
  "reverse of exponential interpolation for normalized x."
  (lambda (x) (log (/ x min) (/ max min))))

(defun m-exp-dev (x max)
  "return a random deviation factor, the deviation being exponentially
interpolated between 1 for x=0 and [1/max..max] for x=127."
  (n-exp-dev (/ x 127) max))

(defun m-lin-dev (x max)
  "return a random deviation offset, the deviation being linearly
interpolated between 0 for x=0 and [-max..max] for x=127."
  (n-lin-dev (/ x 127) max))

(defun m-exp-fn (min max)
  "exp interpolation for midivalues (x = [0..127])"
  (lambda (x) (m-exp x min max)))

(defun m-exp-zero-fn (min max)
  "exp interpolation for midivalues (x = [0..127])"
  (lambda (x) (m-exp-zero x min max)))

(defun m-exp-rd-fn (min max)
  "rounded exp interpolation for midivalues (x = [0..127])"
  (lambda (x) (round (m-exp x min max))))

(defun m-lin-fn (min max)
  "linear interpolation for midivalues (x = [0..127])"
  (lambda (x) (m-lin x min max)))

(defun m-lin-rd-fn (min max)
  "rounded linear interpolation for midivalues (x = [0..127])"
  (lambda (x) (round (m-lin x min max))))

(defun m-exp-rev-fn (min max)
  "exp reverse interpolation fn for midivalues (x = [0..127])"
  (lambda (x) (round (* 127 (log (/ x min) (/ max min))))))

(defun m-exp-zero-rev-fn (min max)
  "exp reverse interpolation fn returning midivalues [0..127]"
  (lambda (x) (if (zerop x) 0
             (round (* 127 (log (/ x min) (/ max min)))))))

(defun m-exp-rd-rev-fn (min max)
  (lambda (x) (round (* 127 (log (/ x min) (/ max min))))))

(defun m-lin-rev-fn (min max)
  (lambda (x) (round (* 127 (/ (- x min) (- max min))))))

(defun m-lin-rd-rev-fn (min max)
  (lambda (x) (round (* 127 (/ (- x min) (- max min))))))


(defmacro with-lin-midi-fn ((min max) &body body)
  "return closure with ipfn bound to a linear interpolation of the
input range 0..127 between min and max."
  `(let ((ipfn (ou:ip-lin ,min ,max 128)))
     (lambda (d2)
       (when (numberp d2)
         ,@body))))

(defmacro with-exp-midi-fn ((min max) &body body)
  "return closure with ipfn bound to an exponential interpolation of
the input range 0..127 between min and max."
  `(let ((ipfn (ou:ip-exp ,min ,max 128)))
     (lambda (d2)
       (when (numberp d2)
         ,@body))))


(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defmacro dovector ((var vector &optional (result nil)) &body body)
  "dolist for vectors. The optional result parameter can be
modified in the body to enable returning a value."
  `(loop
     for ,var across ,vector
     with result = ,result
     do (progn
          ,@body)
     finally (return result)))

#|
(dovector (x #(1 2 3 4 5) '())          ;
  (push x result))

(dovector (x #(1 2 3 4 5) 0)
  (incf result x))

|#

(define-modify-macro multf (&optional (number 1)) *
  "like incf but multiplying instead of adding.")

#|
(defmacro rmprop (plist key &optional default)
  "like getf, but removing the property from the plist."
  `(prog1
       (getf ,plist ,key ,default)
     (remf ,plist ,key)))
|#

(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
      :displaced-index-offset (* row (array-dimension arr 1))))

;;; from cm:

(defun cd (&optional (dirarg (user-homedir-pathname )))
  (let ((dir (if (stringp dirarg)
                 (string-right-trim '(#\/) dirarg)
                 dirarg)))
    (sb-posix:chdir dir)
    (let ((host (pathname-host dir))
          (name (pathname-name dir))
          (path (pathname-directory dir)))
      ;; allow dirs without ending delim "/tmp"
      (when name
        (setq path (append path (list name))))
      (setq *default-pathname-defaults*
            (make-pathname :host host :directory path))
      (namestring *default-pathname-defaults*))))

(defun pwd ()
  (namestring
   (make-pathname :host (pathname-host *default-pathname-defaults*)
                  :directory (pathname-directory
                              *default-pathname-defaults*))))

(defmacro defconst (symbol value)
 `(defconstant ,symbol 
    (or (and (boundp ',symbol) 
             (symbol-value ',symbol))
        ,value)))

(defun r-elt (seq &rest idxs)
  "retrieve element from seq applying the #'elt function recursively
with idxs taken from the rest arg."
  (reduce #'elt idxs :initial-value seq))

(defun getf-or-elt (seq id)
  "depending on the type of id retrieve the element with the #'elt
function or with getf."
  (cond
    ((numberp id) (elt seq id))
    (t (getf seq id))))

(defun r-getf (seq &rest props)
  "recursively traverse nested seq using props as idx. The values for
props can be either numbers using #'elt or keywords/symbols (using
getf)."
  (reduce #'getf-or-elt props :initial-value seq))

(defun index-seq (seq &optional (n 0))
  (cond ((null seq) nil)
        (t (cons (cons n (first seq)) (index-seq (rest seq) (1+ n))))))

(defun mysubseq (seq start &optional end)
  "like subseq, but allow negative values for end, indicating the
number of elems before the end."
  (let ((end (and end (if (< end 0) (+ end (length seq)) end))))
    (subseq seq start end)))

(defun random-elem (seq)
  "return a random element of seq."
  (elt seq (random (length seq))))

(defun port-available-p (portno)
  "check if port is available by issuing shell command. Only works on
Unix with lsof installed."
  (string= ""
           (string-trim '(#\NEWLINE)
                        (with-output-to-string (out)
                          (uiop::run-program (format nil "lsof -i:~d" portno)
                                             :ignore-error-status t
                                             :output out)))))

(defmacro defparameter* (&rest pairs)
  `(progn
     ,@(loop for entry in pairs
             collect (cond
                       ((symbolp entry)
                        `(defparameter ,entry nil))
                       ((third entry)
                        `(defparameter ,(first entry) ,(second entry) ,(third entry)))
                       (t
                        `(defparameter ,(first entry) ,(second entry)))))))

(defmacro defvar* (&rest pairs)
  `(progn
     ,@(loop for entry in pairs
             collect (cond
                       ((symbolp entry)
                        `(defvar ,entry nil))
                       ((third entry)
                        `(defvar ,(first entry) ,(second entry) ,(third entry)))
                       (t
                        `(defvar ,(first entry) ,(second entry)))))))
