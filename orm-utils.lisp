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

(defun every-nth (l n)
  (loop
     for x in l
     for idx from 0
     if (zerop (mod idx n)) collect x))

;;; (every-nth '(1 2 3 4 5 6 7 8 9 10 11) 3)


(defun str-concat (&rest args)
  (apply #'concatenate 'string args))

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

(defun map-tree (fn tree &key (test (lambda (elem) (numberp (first elem)))))
  "map function recursively on all nodes of given tree (represented as a nested list) 
satisfying test. Return the modified tree."
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

(defun calcsndbytes (time &optional (samplerate 44100))
  (let ((ttime (+ (* 60 (car time)) (cadr time))))
    (* ttime samplerate 2)))

;;; (calcsndbytes '(120 0))


(defun mlist (list count)
  (loop for i from 0 below (length list) by count
     collect (loop for n from 0 below count collect (nth (+ n i) list))))

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
	(mapcar modifier (cdr seq) seq)))

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

(defun flatten (list &key (fn #'car))
  "remove all brackets except the outmost in list. Use fn to determine
   where to stop removing brackets.

   Example:

   (flatten '((a b) (((c d) (e f)) (g h)) (i k)))
   -> (a b c d e f g h i k)

   keep one level of brackets:

   (flatten '((a b) (((c d) (e f)) (g h)) (i k)) :fn #'caar)
   -> ((a b) (c d) (e f) (g h) (i k))
   "
  (cond ((null list) nil)
        ((consp (funcall fn list))
         (append (flatten-fn (first list) :fn fn)
                 (flatten-fn (rest list) :fn fn)))
        (t (cons (first list)
                 (flatten-fn (rest list) :fn fn)))))

;;; kept for backwards compatibility:

(defun flatten-fn (list &key (fn #'car))
  "remove all brackets except the outmost in list. Use fn for
   determining the minimum depth of the final list.
   Example:

   (flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)))
   -> (a b c d e f g h i k)

   (flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)) :fn #'caar)
   -> ((a b) (c d) (e f) (g h) (i k))
   "
  (cond ((null list) nil)
        ((consp (funcall fn list))
         (append (flatten-fn (first list) :fn fn)
                 (flatten-fn (rest list) :fn fn)))
        (t (cons (first list)
                 (flatten-fn (rest list) :fn fn)))))

#|
 (flatten-fn '((a b)(((c d) (e f)) (g h)) (i k)))
 -> (a b c d e f g h i k)

 (flatten-fn '((a b)(((c d) (e f)) (g h)) (i k)) :fn #'caar)
 -> ((a b) (c d) (e f) (g h) (i k))
|#

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
      (* slope n))))

;; usage: (defparameter cl1 nil)
;; (setf cl1 (ip-lin 0 1000 10))
;; (defun cl1 (n) (funcall (ip-lin 0.01 1000 10) n))
;; (loop for x from 0 below 10 collect (funcall cl1 x))

(defun fv->ct (fv)
  (* 1200 (log fv 2)))

(defun ct->fv (ct)
  (expt 2 (/ ct 1200)))

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
  (let* ((new-list (copy-tree list))
         (num-normalized (mod num (nconc-get-size new-list)))) ;; innermost function nconcs new-list to an endless list and gets size in one step
    (if (zerop num-normalized) list
      (let* ((newlast (nthcdr (- num-normalized 1) new-list)) ;; determine the new last element of the list (wraps around for nums > (length list) or < 0) and store into variable
             (newfirst (cdr newlast))) ;; the new listhead is the cdr of the last element; also stored in variable
        (progn
          (setf (cdr newlast) nil) ;; set the cdr of the new last element to nil
          newfirst))))) ;; return the new first element (listhead)


;; (rotate '(0 1 2 3 4 5) 3)
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
 (* 20 (log amp 10)))

;;; (amp->db 1) -> 0
;;; (amp->db 0.5) -> -6
;;; (amp->db 2) -> 6

(defun db->amp (db)
  (expt 10 (/ db 20)))

;;; (db->amp 0) -> 1
;;; (db->amp -6) -> 0.5
;;; (db->amp 6) -> 2
;;; (db->amp -60) -> 1/100

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

(defun combinations (seq n)
  "get all n combinations of seq."
  (cond
    ((zerop n) '(()))              ; one combination of zero elements.
    ((null seq) '())               ; no combination from no element.
    (t (nconc (mapcar (lambda (combi) (cons (first seq) combi))
                      (combinations (rest seq) (1- n)))
              (combinations (rest seq) n)))))

;;; (combinations '(1 2 3 4 5) 3)

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

(defun repeat (n elem)
  "return a list with n elems."
  (if (= n 0) ()
      (cons elem (repeat (- n 1) elem))))

(defun range (&rest args)
  "like clojure's range.
   Arities:
   (range end) 
   (range start end) 
   (range start end step) 

   Return a list of nums from start (inclusive) to
   end (exclusive) by step. Start and step are optional args."
  (destructuring-bind (start end step)
      (cond
        ((third args) args)
        ((second args) (list (first args) (second args) 1))
        (t (list 0 (first args) 1)))
    (loop for i from start below end by step collect i)))

;;; (range 1 10 2)

(defun sum-x (n)
 (* n (+ n 1) 1/2))
(defun map-indexed (fn &rest seqs)
  (let ((idx -1))
    (apply #'mapcar
           (lambda (&rest args)
             (apply fn (incf idx) args))
           seqs)))

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
  (with-open-file (out outfile
                       :direction :output
                       :if-exists :supersede)
    (dolist (line seq)
      (format out "~a~%" line))))


(defun slurp (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
       collect (read-from-string line))))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun map-all-pairs (fn seq)
  (loop
     for (b1 . rest) on seq
     append (loop for b2 in rest do (funcall fn b1 b2))))
