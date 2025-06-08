;;;; orm-utils.lisp

(in-package :orm-utils)

(defun system-version (system-designator)
  "Return the version of an installed /asdf/ system or nil if not
bound/existent.

@Arguments
system-designator - A designator acceptable to /asdf:find-system/

@Examples
(system-version :cm) ; => \"2.12.0\"
"
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

(defun path-find-file (fname path)
  "Return the full pathname of the first occurence of fname in path.

@Arguments
fname - String or Pathname of file.
path - List of paths to search.
"
  (let ((fname (pathname fname)))
    (if (uiop:file-exists-p fname)
        (namestring fname)
        (loop
          for dir in path
          for result = (string-trim
                        '(#\NEWLINE)
                        (with-output-to-string (str)
                          #-darwin
                          (uiop:run-program (format nil "find ~a -name ~a -print -quit" dir fname) :output str)
                          #+darwin
                          (uiop:run-program (format nil "find ~a -name ~a -print -quit" dir fname) :output str)))
          while (string= result "")
          finally (return (unless (string= result "") (pathname result)))))))

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
  "Repeatedly execute /body/ until /test/ is non-nil. Returns nil on
exit.

@Arguments
test - Function to test for exiting the while loop.
body - Body to be evaluated repeatedly while /test/ evaluates to non-nil.

@Example
(let ((i 0))
  (ou::while (< (incf i) 10) (print i)))

;; output in the REPL:
1 
2 
3 
4 
5 
6 
7 
8 
9 
nil
"
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
  "Return a list of all elements of /seq/ satisfying /pred/.

@Arguments
seq - A Common Lisp sequence
pred - Function of one element for filtering elements.

@Example
(filter (lambda (e) (< e 9)) '(3 1 12 17 5 4)) ; => (3 1 5 4)
"
  (remove-if-not pred seq))

(defmacro ensure-prop (proplist prop default)
  "Ensure that the property /prop/ exists in /proplist/, otherwise set it
to /default/. Return the value of prop.

@Arguments
proplist - Property list.
prop - Keyword or Symbol denoting the property key to ensure.
default - The value the property should get assigned to if not set.

@Examples
(defvar *proplist* '(:a 10 :b hello :c \"world\"))

(ensure-prop *proplist* :d 5)
;; => 5

*proplist* ; => (:d 5 :a 10 :b hello :c \"world\")

;; As property :a already exists, don't change it and return its
;; current value:

(ensure-prop *proplist* :a 3)
;; => 10

*proplist* ; => (:d 5 :a 10 :b hello :c \"world\")
"
  `(let ((val (getf ,proplist ,prop)))
     (unless val (setf (getf ,proplist ,prop) ,default))
     (or val ,default)))

(defun every-nth (list n &key (offset 0))
  "Return a sublist of /list/ containing every element with an index
being a multiple of /n/.

@Arguments
list - Input list.
n - Positive integer denoting the index distance between elements.
:offset - Positive integer denoting offset into the input list.

@Example
(every-nth '(9 10 11 12 13 14 15 16 17 18 19 20) 3 :offset 1)
;; => (10 13 16 19)
"
  (loop
    for x in (nthcdr offset list)
    for idx from 0
    if (zerop (mod idx n)) collect x))

(defun str-concat (&rest args)
  "concatenate strings.

@Arguments
args - one or more strings to concatenate

@Example
(str-concat \"Hello\" \" World\") ; => \"Hello World\"
"
  (apply #'concatenate 'string args))

(defmacro repeat-format (stream expr num)
  "format /expr/ /num/ times to /stream/

@Arguments
stream - Output stream as in Common Lisp's #'format function.
expr - Expression to format repeatedly.
num - Integer number of repetitions.

@Examples
(repeat-format nil \"la\" 10) ; => \"lalalalalalalalalala\"
"
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
  "Return the first /n/ elems of /seq/."
  (subseq seq 0 n))

(defun rfind (item tree &key (test #'eql))
  "Find /item/ by traversing /tree/ recursively until /test/ called on
/item/ and a tree element returns non-nil. Return item or nil if item
is not found.

@Arguments
item - Any Common Lisp Object.
tree - A list possibly nested.
:test - Function to test for equality between item and a tree element.

@Example
(rfind 'd '(a (b c (a d c) ((g d (e)) h f)))) ; => d 

"
  (if (atom tree)
      (if (funcall test item tree) tree)
      (or (rfind item (first tree) :test test)
          (rfind item (rest tree) :test test))))

(defmacro default (expr default)
  "(if /expr/ /expr/ /default/) without calculating /expr/ twice."
  (let ((myvar (gensym)))
    `(let ((,myvar ,expr))
       (if ,myvar ,myvar ,default))))

(defmacro with-output-to-file ((stream fname &key (if-exists :supersede)) &rest body)
  "Wrapper around /with-open-file/ for output to avoid having to provide
:direction and :if-exists.

@Arguments
stream - Symbol bound to the stream outputting to file.
fname - String or Pathname of output file.
:if-exists - Keyword mapping the /:if-exists/ keyword of /with-open-file/.
"
  `(with-open-file (,stream ,fname :direction :output :if-exists ,if-exists)
     ,@body))

(defun make-adjustable-string ()
  (make-array '(0) :element-type 'character
              :fill-pointer 0 :adjustable t))

(defun last-n (n)
  "Returns a function which will retrieve last n elements of a given
list when applied.

@Arguments
n - Positive Integer denoting the length of the sublist to return.

@Example
(funcall (last-n 5) '(1 2 3 4 5 6 7 8)) -> (4 5 6 7 8)
"
        (lambda (list)
          (let ((len (length list)))
            (if (< len n)
                (error (format nil "(last-n ~d) called with too short list: ~a" n list))
                (nthcdr (- (length list) n) list)))))


(defun dround (num &optional (prec 2))
  "Return a float of /num/ rounded to /prec/ decimal places.

@Arguments
num - The number to round.
prec - Non Negative Integer denoting the number of decimal places.

@Examples
(dround 1/3) ; => 0.33

(dround 1/3 5) ; => 0.33333

(dround 1) ; => 1.0
"
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
  "Map function recursively and non-destructively on all leaf nodes of
given tree (represented as a nested list). Leaf nodes are determind by
applying #'test on the list containing them. If /test/ returns /t/,
the node is considered to be a leaf node. Return the modified tree as
a new structure.

@Arguments
fn - Function to call on the leaf nodes.
tree - List to traverse, possibly nested

@Examples
(map-tree #'print '(1 (2 7 (8 9 ((17 15 (14)) 5 (3))))))
;; => (1 (2 7 (8 9 ((17 15 (14)) 5 (3)))))
;; output in the REPL:
1 
2 
7 
8 
9 
17 
15 
14 
5 
3 

(map-tree (lambda (x) (+ x 100)) '(1 (2 7 (8 9 ((17 15 (14)) 5 (3))))))
;; => (101 (102 107 (108 109 ((117 115 (114)) 105 (103)))))
"
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
  Like mapcon, but uses append instead of nconc. Copied from Peter
  Norvig's AIP book."
  (apply #'append (mapcar fn list)))

(defun get-time (secs &key (prec 2))
  "Convert a /secs/ representing seconds into a list of the form /(hr min
secs)/.

@Arguments
secs - Number representing time in seconds.
prec - Number of digits after the comma of seconds

@Example
(get-time 2753.3) ; => (0 45 53.30005)
"
  (multiple-value-bind (hr mins)
      (floor secs 3600)
      (multiple-value-bind (min secs)
          (floor mins 60)
        (list hr min (dround secs prec)))))

(defun calcsndbytes (hr min sec &key (samplerate 44100) (bytes-per-sample 4))
  "Return the number of bytes (not samples!) from /hr/, /min/ and
/sec/. Samplerate and the number of bytes per sample can be supplied
using the /samplerate/ and /bytes-per-sample/ keywords..

@Arguments
hr - Number of hours.
min - Number of minutes.
hr - Number of seconds.
samplerate - Number of samples per second.
bytes-per-sample - Number of bytes per sample.

@Example
(calcsndbytes 0 1 10) ; => 12348000
"
  (let ((ttime (+ (* 3600 hr) (* 60 min) sec)))
    (floor (* ttime samplerate bytes-per-sample))))

;;; (apply #'calcsndbytes (get-time 120.3))
;;; (calcsndbytes 0 0 120.3)

(defun format-time (hr min sec)
  "Return a time string of the format \"hh:mm:ss.sss\"

@Arguments
hr - Number of hours to output.
min - Number of minutes to output.
sec - Number of seconds to output.

@Examples
(format-time 0 1 1.312) \"00:01:01.312\"

(apply #'format-time (get-time 728.3)) \"00:12:08.300\"
"
  (format nil "~2,'0d:~2,'0d:~:[0~,3f~;~,3f~]" hr min (>= sec 10) sec))

;;; (apply #'format-time (get-time 728.3))

(defun mlist (list count)
  "Similar to group: Divide list into sublists of length count
non-destructively. Last element is padded with nil if list-length is
not a multiple of count. Return the partitioned list.

@Arguments
list - List to be partitioned
count - positive Integer denoting the ength of partitions.

@Example
(mlist '(1 2 3 4 5) 2) => ((1 2) (3 4) (5 nil))
"
  (loop for i from 0 below (length list) by count
     collect (loop for n from 0 below count collect (elt list (+ n i)))))

;;; (mlist '(1 2 3 4 5) 2) -> ((1 2) (3 4) (5 NIL))

#|

(defun integrate (seq)
  (reducing #'+ seq))


(integrate '(0 1 2 3 4))
|#

(defun integrate (seq &key (modifier #'+) (start (elt seq 0)))
  "Return a running sum (or any other /modifier/ function) of /seq/.

@Arguments
seq - Proper sequence to integrate.
:modifier - Function to apply to all elements accumulationg the results.
:start - Number denoting the start value.

@Examples
(integrate '(0 2 1 4 5)) ; => (0 2 3 7 12)

(integrate '(0 2 1 4 5) :start 10) ; => (10 12 13 17 22)

(integrate '(1 2 3 2 4) :modifier #'*) ; => (1 2 6 12 48)

(integrate '(1 2 3 2 4) :modifier #'*) ; => (1 2 6 12 48)


@See-also
differentiate
"
  (declare (type sequence seq))
  (let (last)
    (map (type-of seq) (lambda (x) (if last (setf last (funcall modifier last x))
                                  (setf last start)))
         seq)))


;; (defun differentiate (liste)
;;   (cons (car liste)
;;         (loop 
;;            for i in liste
;;            for j in (cdr liste)
;;            collect (- j i))))

(defun differentiate (seq &key (modifier #'-) (start (elt seq 0)))
  "Return differences or the results of applying /modifier/ to subsequent
elements of /seq/.

@Arguments
seq - Proper sequence to integrate.
:modifier - Function to apply to all elements accumulationg the results.
:start - Number denoting the start value.

@Examples
(differentiate '(0 2 3 7 12)) ; => (0 2 1 4 5)

(differentiate #(0 2 3 7 12) :start 3) ; => #(3 2 1 4 5)

(differentiate '(1 2 6 12 48) :modifier #'/) ; => (1 2 3 2 4)

(differentiate (integrate '(17 2 4))) ; => (17 2 4)

@See-also
integrate
"
  (declare (type sequence seq))
  (let (last)
    (map (type-of seq) (lambda (x)
                         (if last
                             (prog1
                                 (funcall modifier x last)
                               (setf last x))
                             (prog1 start
                               (setf last x))))
         seq)))

#|
 (differentiate #(1 3 2 5 6 4))
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

(defun flatten-fn (list &key (test #'atom) (key #'identity))
  "Remove all brackets except the outmost in /list/. Use /test/ and /key/
to determine where to stop removing brackets.

@Arguments
list - input List.
test - Function applied to each element of list to test for the end of flattening.
key - Function applied to each element of list before testing. 

@Examples

(flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)))
;; -> (a b c d e f g h i k)

;; keep one level of brackets:

(flatten-fn '((a b) (((c d) (e f)) (g h)) (i k)) :key #'car)
;; -> ((a b) (c d) (e f) (g h) (i k))

@See-also
flatten
"
  (cond ((null list) nil)
        ((funcall test (funcall key list)) list)
        ((funcall test (funcall key (first list)))
         (cons (first list)
               (flatten-fn (rest list) :test test :key key)))
        ((consp (first list))
         (append (flatten-fn (first list) :test test :key key)
                 (flatten-fn (rest list) :test test :key key)))
        (t (append (flatten-fn (first list) :test test :key key)
                   (flatten-fn (rest list) :test test :key key)))))



#|
 (flatten-fn '(1 2 3 (a b)(((c d) (e f)) (g h)) (i k)))
 -> (a b c d e f g h i k)

 (flatten-fn '((a b)(((c d) (e f)) (g h)) (i k)) :fn #'caar)
 -> ((a b) (c d) (e f) (g h) (i k))

|#

(defun flatten (form)
  "Remove all brackets except the outmost from /form/. Non-recursive,
non-stack version from Rosetta Code.

@Arguments
form - A Common Lisp form.

@Examples

(flatten '((a b) (((c d) (e f)) (g h)) (i k)))
;; -> (a b c d e f g h i k)

@See-also
flatten-fn
"
  (do* ((result (list form))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

;;; calc fibonacci number directly:

(defun fibonacci (n)
  "Calculate the /n/-th element of the Fibonacci series. The function is
not recursive, but calculates the value directly, running in constant
time.

@Arguments
n - Non Negative Integer denoting the index of the Fibonacci series.

@Example
(mapcar #'fibonacci (range 12)) ; => (1 1 2 3 5 8 13 21 34 55 89 144)
"


  (declare (integer n))
  (let ((sr5 (sqrt 5)))
    (round (* (/ 1 sr5) 
              (- (expt (/ (+ sr5 1) 2) (1+ n)) 
                 (expt (/ (- 1 sr5) 2) (1+ n)))))))

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

#|
(defun ip-exp (min max &optional (steps 2))
  (let ((st (- steps 1))
        (base (/ max min)))
    (lambda (n)
      (* min (expt base (/ n st))))))
|#

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

(defun fr->ct (fr)
  "Return the Midicents interval of the frequency ratio /fr/.

@Arguments
fr - Positive Number denoting the frequency ratio of the interval.

@Examples
(fr->ct 2) ;; => 12.0

(fr->ct 4/5) ;; => -3.863137

(fr->ct 3/2) ;; => 7.01955

(fr->ct 1/2) ;; => -12.0

(mapcar #'fr->ct (range 1 17))
;; => (0.0 12.0 19.01955 24.0 27.863136 31.01955 33.68826 36.0 38.0391 39.863136
;;     41.51318 43.01955 44.405277 45.68826 46.882687 48.0)

@See-also
ct->fr
"  (* 12 (log fr 2)))

(defun ct->fr (ct)
  "Return the frequency ratio of the Midicents interval /ct/.

@Arguments
ct - The interval in Midicents.

@Examples
(ct->fr 12) ;; => 2

(ct->fr 1) ;; => 1.0594631

(ct->fr 7) ;; => 1.4983071

(ct->fr -12) ;; => 1/2

(mapcar #'ct->fr (range 13))
;;  => (1 1.0594631 1.122462 1.1892071 1.2599211 1.3348398 1.4142135 1.4983071
;;      1.587401 1.6817929 1.7817974 1.8877486 2)

@See-also
fr->ct
"
  (expt 2 (/ ct 12)))

(defun mtof (m &key (tuning-base 440))
    "Convert pitch in Midicts to frequency in Hz.

@Arguments
midi-value - Positive Number denoting Pitch in Midicents.
:tuning-base - Frequency of A4 in Hz.

@Examples
(mtof 69) ; => 440

(mtof 60.5) ; => 269.29178

(mtof 69 :tuning-base 415) ; => 415

@See-also
ftom
"
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

(defun rotate (seq &optional (n 1))
  "Rotate /seq/ by /num/ elems (to the right). /num/ can be negative. If
/num/ is larger than the list size it will wrap around as if the
rotation was called recursively num times.

@Arguments
seq - Proper sequence to rotate.
num - Integer number of rotations.

@Examples
(rotate '(dog bird lion cat horse) 1) ; => (horse dog bird lion cat)
(rotate #(dog bird lion cat horse) -1)  ; => (bird lion cat horse dog)

(rotate '(dog bird lion cat horse) 4733) ; => (lion cat horse dog bird)
"
  (let ((n (mod n (length seq))))
    (concatenate (type-of seq) (subseq seq n) (subseq seq 0 n))))

(defun rotate-list (list &optional (num 1))
  "Rotate /list/ by /num/ elems (to the right). /num/ can be negative. If
/num/ is larger than the list size it will wrap around as if the
rotation was called recursively num times.

@Arguments
list - List to rotate
num - Integer number of rotations.

@Examples
(rotate '(dog bird lion cat horse) 1) ; => (horse dog bird lion cat)
(rotate '(dog bird lion cat horse) -1)  ; => (bird lion cat horse dog)

(rotate '(dog bird lion cat horse) 4733) ; => (lion cat horse dog bird)
"
  (let* ((new-list (copy-tree list))
         (num-normalized (mod (* -1 num) (nconc-get-size new-list)))) ;; innermost function nconcs new-list to an endless list and gets size in one step
    (if (zerop num-normalized) list
      (let* ((newlast (nthcdr (- num-normalized 1) new-list)) ;; determine the new last element of the list (wraps around for nums > (length list) or < 0) and store into variable
             (newfirst (cdr newlast))) ;; the new listhead is the cdr of the last element; also stored in variable
        (setf (cdr newlast) nil) ;; set the cdr of the new last element to nil
        (values newfirst))))) ;; return the new first element (listhead)

;; (rotate '(0 1 2 3 4 5) -3) -> (3 4 5 0 1 2)
;;
;; (defparameter test '(0 1 2 3 4 5))
;;
;; (rotate test 3)
;; test

(defun transpose-list (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

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

(defun group-by (list group-lengths)
  "Partition /list/ into sublists of lengths given by /group-lenghts/
cyclically.

@Arguments
list - The list to partition.
group-lenghts - List of Positive Integers denoting the sequence of lengths of the partitions.

@Example
(group-by '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6) '(2 3 5))
;; => ((1 2) (3 4 5) (6 7 8 9 1) (2 3) (4 5 6) (7 8 9 1 2) (3 4) (5 6))
"
  (let* ((tmp (copy-list group-lengths))
         (gseq (nconc tmp tmp)))
    (labels ((rec (list acc)
               (let* ((n (first gseq))
                      (rest (nthcdr n list)))
                 (if (zerop n) (error "zero length"))
                 (setf gseq (rest gseq))
                 (if (consp rest)
                     (progn
                       (rec rest (cons (subseq list 0 n) acc)))
                     (nreverse (cons list acc))))))
      (if list (rec list nil) nil))))

;;; (group-by '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6) '(2 3 5))
;;; => ((1 2) (3 4 5) (6 7 8 9 1) (2 3) (4 5 6) (7 8 9 1 2) (3 4) (5 6))

(defun group-by-key (source &key (test #'=) (key #'car))
  "Group elems of /source/ into sublists depending on /test/ and
/key/. Source has to be sorted according to test!"
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

(defun repeated (n fn)
  "Return a function which applies a given function /fn/ /n/ times onto
itself. /fn/ must be a function of at least one argument which returns
one argument. The return value of the function replaces the first
argument in the next recursive call, leaving all other arguments as
they were.

@Arguments
n - Integer number of repetitions.
fn - Function to be applied to itself.

@Examples
(funcall (repeated 4 (lambda (x) (* x 2))) 1)  ; => 16

(funcall (repeated 4 (lambda (x) (* x 2))) 3) ; => 48

@See-also
do-repeated
"
  (cond ((= n 1)
         (lambda (&rest args) (apply fn args)))
        (t (lambda (&rest args)
             (apply fn (cons
                        (apply (repeated (- n 1) fn) args)
                        (rest args)))))))

;;; Example:

;;; (funcall (repeated 4 (lambda (x y) (* x y))) 1 2) ; -> 16

(defun permute (list permutation)
  "Return a permutation of /list/ according to the indexes in
/permutation/.


@Arguments
list - List of elements to be permuted.
permutation - List of permutation indexes.

@Examples

(permute '(1 2 3 4 5) '(3 1 4 2 0)) ; => (4 2 5 3 1)

@Note
For a valid permutation the /permutation/ index list should contain
all integer indexes of list starting from zero. In that case, length
of /list/ is equal to the length of /permutation/. If it is shorter,
an error occurs, if it is longer, not all elements of /list/ are
returned.
"
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

(defun do-repeated (n fn &rest args)
  "Recursively apply /fn/ to /args/ /n/ times.

@Arguments
n - Integer number of repetitions.
fn - Function to apply.

@Examples
(do-repeated 4 (lambda (x) (* x 2)) 1) ; => 16

(do-repeated 4 (lambda (x) (* x x)) 2)  ; => 65536

(do-repeated 6 (lambda (list) (cons 1 list)) '()) ; => (1 1 1 1 1 1)

@See-also
repeated
"
  (apply (repeated n fn)
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
  "Return number of beats for a given time /secs/ in seconds. Keys are
/:tempo/ (default '(1/4 60)) and a /unit/ to count (default 1/4)."
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
  "Return dB value of linear amplitude /amp/. An amplitude of 0 returns a
dB value of -100.

@Arguments
amp - Positive Integer denoting linear amplitude.

@Example

(amp->db 1) ; => 0.0
(amp->db 0) ;= -100
"
  (if (zerop amp) -100
      (* 20 (log (abs amp) 10))))

;;; (amp->db 1) -> 0
;;; (amp->db 0.5) -> -6
;;; (amp->db 2) -> 6

(defun db->amp (db)
  "Return amp value of dB value /db/. The dB value is clipped below -100
and returns the amplitude 0.

@Arguments
amp - Positive Integer denoting amplitude.

@Example

(db->amp 0) ; => 1
(db->amp -6) ; => 0.5011872
(db->amp -100) ; => 0
"
  (if (<= db -100) 0
      (expt 10 (/ db 20))))

;;; (db->amp 0) -> 1
;;; (db->amp -6) -> 0.5
;;; (db->amp 6) -> 1.9952623
;;; (db->amp -60) -> 1/100

(defun clip (val min max)
  "Clip val into the range [min..max].

@Arguments
val - Number to be clipped.
min - Number denoting minimum bound.
min - Number denoting maximum bound.

@Example
(clip -10 0 3) ; => 0

(clip 10 0 3) ; => 3

(clip 1.73 0 3) ; => 1.73
"
  (min max (max min val)))

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
  "Partition /seq/ into sublists based on a predicate called on successive
elements. /pred/ is a function of two args, an element of the seq and
its successor. If pred returns non-nil, a new subseq is started after
the current element. The result contains all elements of the original
seq in orginal order.

@Example

(partition-seq '(1 2 4 5 6 8 9) #'(lambda (x y) (> (- y x) 1))) 
;; => ((1 2) (4 5 6) (8 9))

(partition-seq '(1 2 4 5 6 8 9) #'(lambda (x y) t)) 
;; => ((1) (2) (4) (5) (6) (8) (9))

"
  (loop 
    with res = () 
    with currlist = () 
    for (curr next) on seq
    do (progn
         (if (and next (funcall pred curr next)) 
             (progn
               (push (reverse (push curr currlist)) res)
               (setf currlist nil))
             (push curr currlist)))
    finally (return (reverse (push (reverse currlist) res)))))

(defmacro with-shadowed-variable ((var) &rest body)
  "Shadow /var/ in the local scope of /body/. /var/ should be bound
before entering /with-shadowed-variable/.

@Arguments
var - Symbol of variable to shadow
body - Body for the scope of the shadowing.

@Example
(defvar *myvar* 2) ; => *myvar*

(defun return-myvar ()
 *myvar*)

(with-shadowed-variable (*myvar*)
  (setf *myvar* 10)
  (return-myvar))

;; => 10

*myvar* ; => 2
"
  `(let ((,var ,var))
     ,@body))

(defun make-quantlist (vals)
  "Return the sorted list of quantization points in fractions of a beat
[0..1] for a list of the beat division numbers to be considered,
supplied in /vals/.

@Arguments
vals - List of integer beat-divisions to be collected.

@Examples
(make-quantlist '(4)) ; => (0 1/4 1/2 3/4 1)

(make-quantlist '(3 4)) ; => (0 1/4 1/3 1/2 2/3 3/4 1)

(make-quantlist '(3 4 5)) ; => (0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1)

@See-also
quantize-time
"
  (sort
   (remove-duplicates
    (reduce
     (lambda (acc x) (append acc (loop for n from 1 below x collect (/ n x))))
     vals :initial-value '(0 1)))
   #'<))

(defun quantize-time (val &optional (quantlist (make-quantlist '(3 4 5))))
  "Quantize the fractional part of /val/ to a quantization list
/quantlist/ of possible quantization points in the range [0..1].

@Arguments
val - The value to be quantized.
quantlist - Sorted list of possible quantization points in the range [0..1].

@Examples
(quantize-time 1/7 (make-quantlist '(3 4 5))) ; => 1/5

(quantize-time 37/7 (make-quantlist '(3 4 5))) ; => 21/4

(quantize-time 17/7 (make-quantlist '(3 4 5))) ; => 12/5

(quantize-time 17/7 (make-quantlist '(3 4))) ; => 5/2 

@See-also
make-quantlist
"
  (multiple-value-bind (int frac) (floor val)
    (loop for (last curr) on quantlist until (<= last frac curr)
          finally (return (+ int (if (<= (- frac last) (- curr frac)) last curr))))))

;;; (quantize 14.71) -> 59/4 (14.75)


(defun insert (elem result &key (key #'first) (test #'eq))
  "Helper function for splice: Inserts an element into a sublist of
result, if one exists with key elements equal to the key of elem,
otherwise it appends a new sublist containing elem at the end of
result.  The function returns the updated result."
  (cond ((null result) (list (list elem)))
        ((funcall test (funcall key elem) (funcall key (caar result)))
         (cons (cons elem (first result)) (rest result)))
        (t (cons (first result) 
                 (insert elem (rest result) :key key :test test)))))

(defun splice (list &key (key #'first) (test #'eq))
    "Return a list of all sublists containing elements mutually satisfying
the /test/ predicate.

@Arguments
list - List to splice
key - Function applied to each element of list before testing.
test - Function to determine equality between two elements.

@Examples
(splice '((0 1) (4 7) (7 2) (0 3) (4 5) (1 3)))
 ; => (((0 1) (0 3)) ((4 7) (4 5)) ((7 2)) ((1 3)))

(splice '((0 1) (4 7) (7 2) (0 3) (4 5) (1 7)) :key #'second)
 ; => (((0 1)) ((4 7) (1 7)) ((7 2)) ((0 3)) ((4 5)))
"
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
  "Return all Elements of /list/ which occur more than once with respect
to the /test/ predicate. If /once/ is non-nil, return each duplicate
element only once.

@Arguments
list - List being examined.
test - Function to determine equality of elements.
once - Boolean to determine if only one of the duplicate elements is returned.

@Examples
(get-duplicates '(0 1 3 2 4 3 9 3 1 3 4 2 3)) ; => (1 3 2 4 3 3 3)

(get-duplicates '(0 1 3 2 4 3 9 3 1 3 4 2 3) :once t)  ; => (1 2 4 3)
"
  (let ((all-duplicates
          (loop for x on list
             append (if (member (first x) (rest x) :test test) (list (first x))))))
    (if once 
        (remove-duplicates all-duplicates :test test)
        all-duplicates)))

;; (get-duplicates '(1 3 2 4 1 2 5 1) :once t)

(defun all-permutations (list &key (test #'eql) (max-length 10))
  "Get all permutations of /list/. Make sure to supply a /test/
function in case the elements can't be compared with #'eql, otherwise
the function will blow the stack. /max-length/ is the maximum length
of /list/ accepted. This serves as a safety measure to avoid making
the lisp process unresponsive due to an excessive number of
permutations.

@Arguments
list - List of elements to be permuted.
:test - Function to test for equality of elements in list.
:max-length - Positive Integer denoting maximum length of list accepted.

@Examples
(all-permutations (range 4))
;; => ((0 1 2 3) (0 1 3 2) (0 2 1 3) (0 2 3 1) (0 3 1 2) (0 3 2 1) (1 0 2 3)
;; (1 0 3 2) (1 2 0 3) (1 2 3 0) (1 3 0 2) (1 3 2 0) (2 0 1 3) (2 0 3 1)
;; (2 1 0 3) (2 1 3 0) (2 3 0 1) (2 3 1 0) (3 0 1 2) (3 0 2 1) (3 1 0 2)
;; (3 1 2 0) (3 2 0 1) (3 2 1 0))

(all-permutations (range 20))
;;
;; => Error: list to be permuted exceeds maximum length.
"
  (if (> (length list) max-length)
      (error "list to be permuted exceeds maximum length.")
      (if (null list) (list nil)
          (mapcan (lambda (elem)
                    (mapcar (lambda (p)
                              (cons elem p))
                            (all-permutations (remove elem list :test test))))
                  list))))

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
  "Get all n combinations of seq."
  (cond
    ((zerop n) '(()))              ; one combination of zero elements.
    ((null seq) '())               ; no combination from no element.
    (t (nconc (mapcar (lambda (combi) (cons (first seq) combi))
                      (combinations (rest seq) (1- n)))
              (combinations (rest seq) n)))))

;;; (combinations '(1 2 3 4 5) 2)

(defun reverse-all (list)
  "Recursively reverse list and all its sublists.
@Arguments
list - The list to recursively reverse.

@Example
(reverse-all '(1 (2 3) (4 (5 (6 7) 8) 9))) ; => ((9 (8 (7 6) 5) 4) (3 2) 1)
"
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

#|
(defun append-into (collector value)
  "Collect VALUE into COLLECTOR, returning VALUE.

If COLLECTOR is something made by MAKE-COLLECTOR, do the right thing.
If it is a function (such as the local functions defined by COLLECTING
/ WITH-COLLECTORS), simply call it with the value.

This is the closest equivalent to Interlisp's TCONC."
  (etypecase collector
    (function
     (funcall collector value))
    (cons
     (let ((it (list value)))
       (if (null (cdr collector))
           (setf (car collector) it
                 (cdr collector) it)
         (setf (cdr (cdr collector)) it
               (cdr collector) it))
       value))))

(defun call/appending (f n &optional (tail '()))
  "Call function /f/ /n/ times, with idx [0..n-1] as argument,
collecting its results. Return results with /tail/ appended.

@Arguments

f - Function of one argument (an integer in the range [0..n])
n - Positive integer
tail - A list collected into by prepending to it

@Examples

(call/collecting (lambda (x) (* x x)) 4) ; => (0 1 4 9)

(call/collecting (lambda (x) (1+ x)) 4 '(hi)) ; => (1 2 3 4 hi)

@See-also
v-collect
"
  (let ((c (make-collector)))
    (dotimes (i n (collector-contents c tail))
      (append-into c (funcall f i)))))
|#

(defun call/collecting (f n &optional (tail '()))
  "Call function /f/ /n/ times, with idx [0..n-1] as argument,
collecting its results. Return results with /tail/ appended.

@Arguments

f - Function of one argument (an integer in the range [0..n])
n - Positive integer
tail - A list collected into by prepending to it

@Examples

(call/collecting (lambda (x) (* x x)) 4) ; => (0 1 4 9)

(call/collecting (lambda (x) (1+ x)) 4 '(hi)) ; => (1 2 3 4 hi)

@See-also
v-collect
"
  (let ((c (make-collector)))
    (dotimes (i n (collector-contents c tail))
      (collect-into c (funcall f i)))))

(defmacro v-collect ((v n &optional (tail '())) &rest body)
  "Return a list of /n/ elems prepended to /tail/ by evaluating /body/
/n/ times with the symbol /v/ bound to the iteration index in the
lexical scope of body.

@Arguments
v - Symbol used as variable name.
n - Integer indicating the number of iterations.
body - Function body being evaluated n times.
@Examples

(v-collect (n 10) (* n n)) ;-> (0 1 4 9 16 25 36 49 64 81)

@See-also
call/collecting
v-append
"
  `(call/collecting (lambda (,v) 
                      (declare (ignorable ,v))
                      ,@body)
                    ,n ,tail))

;;; (v-collect (n 10) (* n n)) ;-> (0 1 4 9 16 25 36 49 64 81)

(defmacro v-append ((v n &optional (tail '())) &rest body)
  "Return a list of /n/ elems appended to each other and prepended to
/tail/ by evaluating /body/ /n/ times with the symbol /v/ bound to the
iteration index in the lexical scope of body. The body has to return a
list.

@Arguments
v - Symbol used as variable name.
n - Integer indicating the number of iterations.
body - Macro body being evaluated n times. Needs to return a list.
@Examples

(v-append (n 5) (list :num (* n n))) ;-> (:num 0 :num 1 :num 4 :num 9 :num 16)

@See-also
call/collecting
v-collect
"
  `(apply #'append (v-collect (,v ,n (list ,tail)) ,@body)))

(defun repeat (n elem)
  "return a list with n occurences of elem. All occurences of elem are
#'eq to each other.

@Arguments
n - Integer indicationg the number of iterations
elem - Any Lisp Object to be repeated.

@Examples
(repeat 10 5) ;-> (5 5 5 5 5 5 5 5 5 5)
"
  (v-collect (v n) elem))

;;; (repeat 10 5)

(defun range (&rest args)
  "Like clojure's range: Return a list of nums from start (inclusive) to
end (exclusive) by step. Start and step are optional args defaulting
to 0 and 1 respectively.

Arities:

(range end) 

(range start end) 

(range start end step) 

@Examples

(range 8) ; => (0 1 2 3 4 5 6 7) 

(range 3 9) ; => (3 4 5 6 7 8) 

(range 1 10 2) ; => (1 3 5 7 9)

@Note
Unlike clozure's range function, this range function is not
lazy: As a precaution (range) will return the empty list.
"
  (destructuring-bind (start end step)
      (cond
        ((third args) args)
        ((second args) (list (first args) (second args) 1))
        (t (list 0 (or (first args) 0) 1)))
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
  "Map /fn/ over /seqs/ with incrementing zero-based idx. The idx will
get supplied as first arg to /fn/. /result-type/ serves the same
purpose as in #'map.

@Arguments
result-type - Result type to return. If nil, don't return a result.
fn - Function to map over sequences. Needs to accept /(+ 1 (length
seqs))/ arguments.
seqs - One or more sequences where mapping gets applied, similar to map.

@Example

(map-indexed 'list #'list '(a b c d e)  '(20 10 30 50 40))
;; => ((0 a 20) (1 b 10) (2 c 30) (3 d 50) (4 e 40))
"
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
  "Print /seq/ to /outfile/, each element on a new line."
  (with-open-file (out outfile
                       :direction :output
                       :if-exists :supersede)
    (format out "~{~S~%~}" seq)))


(defun slurp (file)
  "Return contents of file as a list of all lines read individually by
the lisp reader."
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
       collect (read-from-string line))))

(defun slurp-string (file)
  "Return contents of file as a string."
  (with-open-file (stream file)
    (with-output-to-string (str)
      (do ((line (read-line stream nil) (read-line stream nil)))
          ((null line) nil)
        (format str "~a~%" line)))))

(defun make-keyword (name)
  "Return a keyword from /name/.

@Arguments
name - String to intern

@Example
(make-keyword \"Hello\") => :hello
"
  (values (intern (string-upcase name) "KEYWORD")))

(defun map-all-pairs (return-type fn list)
  "Execute /fn/ on all possible pairs of two different elements of
/list/. The pairs are given to fn in the order of appearance in the
list. /return-type/ serves the same purpose as in #'map.

@Arguments
return-type - A Sequence type or nil.
fn - Function of two arguments called on all pairs.
list - List containing all elements to which fn gets applied pairwise.

@Example
(map-all-pairs 'list #'list '(1 2 3 4 5))
;; => ((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5))
"
  (loop
    with acc = '()
    for (b1 . rest) on list
    do (loop for b2 in rest do (if return-type
                                   (push (funcall fn b1 b2) acc)
                                   (funcall fn b1 b2)))
    finally (return (if return-type (coerce (reverse acc) return-type)))))

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
  "Return a string of the current time in the format
  /\"yyyy-mm-dd-hr-min-sec\"/"
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
  "Push form to list if form evaluates to non-nil. Return the modified
list or nil if form evaluates to nil.

@Arguments
form - Form to be pushed to list.
list - List pushed into.

@Examples
(defvar *test* '()) ; => *test*

(push-if 4 *test*) ; => (4)

(push-if (eq 2 3) *test*) ; => nil

*test* ; => (4)
"
  (let ((result (gensym "result")))
    `(let ((,result ,form))
       (if ,result (push ,result ,list)))))

(defparameter *last-random-state* nil)

(defun memorize-random-state ()
  (setf *last-random-state* (make-random-state *random-state*)))

(defun recall-random-state ()
  (setf *random-state* *last-random-state*))

(defun count-elements-generic-test (list &key (test #'eql))
  "Count the number of occurences of all different elems in
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
  "Count the number of occurences of all mutually equal elems in
/seq/ extracted from its items according to the /key/ function and
satisfying the /test/ function as predicate. Return the results as
list with sublists of the form (elem count) for each elem, optionally
sorted according to the setting of /sort/.

@Arguments
seq - A Common Lisp Sequence.
:test - Function accepted as test function by #'make-hash-table.
:key - Function to extract the key to compare from each element.
:sort - Boolean indicationg whether and how to sort the
results. Possible Values are:
- /:from-end/
- /t/
- /nil/

@General
If /:sort/ is /nil/, result returns the items in the order of their
first occurence, if /:sort/ is /:from-end/, they are returned in
reverse order of occurence, if /:sort/ is /t/, they are either sorted
by their value, if all elems are numbers or by the number of occurences
otherwise.

@Examples
(count-elements '(1 3 2 6 5 4 3 8 1 3 5 2 4 3 6 5 3 3 4 1))
;; => ((1 3) (2 2) (3 6) (4 3) (5 3) (6 2) (8 1))

(count-elements '(1 3 2 6 5 4 3 8 1 3 5 2 4 3 6 5 3 3 4 1) :sort :from-end)
;; => ((8 1) (4 3) (5 3) (6 2) (2 2) (3 6) (1 3))

(count-elements '(1 3 2 6 5 4 3 8 1 3 5 2 4 3 6 5 3 3 4 1) :sort t)
;; => ((1 3) (2 2) (3 6) (4 3) (5 3) (6 2) (8 1))

(count-elements '(a b a d e c d a e d e b d f d e) :sort t)
;; => ((d 5) (e 4) (a 3) (b 2) (f 1) (c 1))

(count-elements '((a 10) (b 11) (a 12) (d 13)) :key #'first :sort t)
;; => ((a 2) (d 1) (b 1))
"
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


(count-unique-elements '(1 3 2 4 3 2 1 5 4 3 6 5 7 "hallo" 5 "hallo" 6 "peng" 7 4 6 5 7 2 3) :test #'equal)

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
  "Destructively remove props from property list /proplist/ and return
it.

@Arguments
proplist - Property list.
props - One or more properties to delete.

@See-also
do-proplist
do-proplist/collecting
get-prop
get-props-list
map-proplist
with-props
"
  (mapc (lambda (prop) (remf proplist prop)) props)
  proplist)

(defun get-props-list (proplist props &key (force-all nil))
  "Return a new property list containing /props/ and their values
extracted from /proplist/. Properties not present in proplist are
ignored. If /force-all/ is non-nil, also return properties not present
in proplist with nil as value.

@Arguments
proplist - Property list.
props - Properties to extract from proplist.
:force-all - Boolean to indicate if non-present props should get included in result.

@Examples
(get-props-list '(:a 1 :b 2 :c 3 :d \"Foo\") '(:d :a)) ; => (:d \"Foo\" :a 1)

(get-props-list '(:a 1 :b 2 :c 3 :d \"Foo\") '(:a :e)) ; => (:a 1)

(get-props-list '(:a 1 :b 2 :c 3 :d \"Foo\" :a 4) '(:a :e) :force-all t)  ; => (:a 1 :e nil)

@See-also
delete-props
do-proplist
do-proplist/collecting
get-prop
map-proplist
with-props
"
  (reduce (lambda (seq prop) (let ((val (getf proplist prop :not-supplied)))
                          (if (eql val :not-supplied)
                              (if force-all
                                  (list* prop nil seq)
                                  seq)
                              (list* prop val seq))))
          (reverse props)
          :initial-value nil))

(defmacro with-props (vars proplist &body body)
  "Like with-slots but using a property list instead of a class
instance. The properties in the /proplist/ to be used need to have a
symbol or a keyword as a key. /vars/ is a list of symbols bound to the
corresponding property values in the lexical scope of /body/. Each
element of vars corresponds to a key in proplist either being the
binding symbol itself or a keyword, derived by prepending a colon to
the binding symbol.

@Arguments
vars - List of symbols of the Properties to use in the lexical scope of body.
proplist - Property list containing bound properties.
body - The body in which the vars are bound.

@Example
(with-props (a b c) '(:a 1 :b 2 c 3)
  (list a b c))
;; => (1 2 3)
@See-also
delete-props
do-proplist
do-proplist/collecting
get-prop
get-props-list
map-proplist
"
  `(let ,(mapcar
          (lambda (sym) (list sym `(or (getf ,proplist ,(intern (symbol-name sym) :keyword))
                                  (getf ,proplist ',sym))))
          vars)
     ,@body))

;;; (ou:with-props (amp keynum) '(:amp 1 :keynum 60) (list amp keynum)) => (1 60)

(defmacro map-proplist (fn proplist)
  "Like mapcar but traversing a property list. /fn/ has to accept two
values, the key and the value of each property in the proplist.

@Arguments
fn - Function to apply to all entries of the property list.
proplist - Property list to traverse.

@Example
(map-proplist #'list '(:a 2 :b 5 :c 4)) ; => ((:a 2) (:b 5) (:c 4))

@See-also
delete-props
do-proplist
do-proplist/collecting
get-prop
get-props-list
with-props
"
  `(loop for (key value) on ,proplist by #'cddr
         collect (funcall ,fn key value)))

;;; (map-proplist #'list '(:a 2 :b 5 :c 4)) => ((:a 2) (:b 5) (:c 4))

(defmacro do-proplist ((keysym valuesym) proplist &body body)
  "Like dolist but traversing a property list. All keys and values of
/proplist/ are bound to the symbols /keysym/ and /valuesym/ in the
lexical scope of /body/.

@Arguments
keysym - Symbol bound to all keys of the property list.
valuesym - Symbol bound to all values of the property list.
proplist - Property list to be traversed.

@Examples
(do-proplist (key value) '(a 1 b 2 c 3 d 4)
  (format t \"key: ~a, value: ~a~%\" key value)) ;  => nil

;; Output in REPL:
;;
;; key: a, value: 1
;; key: b, value: 2
;; key: c, value: 3
;; key: d, value: 4

(let ((proplist '(a 1 b 2 c 3 d 4)))
  (do-proplist (key value) proplist
    (setf (getf proplist key) (incf value 10)))
  proplist)
;; => (a 11 b 12 c 13 d 14)

@See-also
delete-props
do-proplist/collecting
get-prop
get-props-list
map-proplist
with-props
"
  `(loop for (,keysym ,valuesym) on ,proplist by #'cddr
         do ,@body))

#|
(export 'with-proplist/collecting 'orm-utils)
(defmacro with-proplist/collecting ((keysym valuesym) proplist &body body)
  "Like do-proplist but collecting the result. All keys and values of
/proplist/ are bound to the symbols /keysym/ and /valuesym/ in the
lexical scope of /body/.

@Arguments
keysym - Symbol bound to all keys of the property list.
valuesym - Symbol bound to all values of the property list.
proplist - Property list to be traversed.

@Examples
(do-proplist/collecting (key val) '(:a 2 :b 5 :c 4)
  (list key (1+ val)))
;; => ((:a 3) (:b 6) (:c 5))

@See-also
delete-props
do-proplist
get-prop
get-props-list
map-proplist
with-props
"
  `(loop for (,keysym ,valuesym) on ,proplist by #'cddr
         collect ,@body))
|#

(defun hash-table-contents (hash-table)
  "Return all key/value pairs of /hash-table/ in a list."
  (let ((res nil))
    (maphash (lambda (key val) (push (list key val) res)) hash-table)
    res))

(defmacro do-proplist/collecting ((keysym valuesym) proplist &body body)
  "Like do-proplist but collecting the result. All keys and values of
/proplist/ are bound to the symbols /keysym/ and /valuesym/ in the
lexical scope of /body/.

@Arguments
keysym - Symbol bound to all keys of the property list.
valuesym - Symbol bound to all values of the property list.
proplist - Property list to be traversed.

@Examples
(do-proplist/collecting (key val) '(:a 2 :b 5 :c 4)
  (list key (1+ val)))
;; => ((:a 3) (:b 6) (:c 5))

@See-also
delete-props
do-proplist
get-prop
get-props-list
map-proplist
with-props
"
  `(loop for (,keysym ,valuesym) on ,proplist by #'cddr
         collect ,@body))

;;; (with-proplist/collecting (key val) '(:a 2 :b 5 :c 4) (list key (1+ val))) => ((:a 3) (:b 6) (:c 5))

(defun get-prop (proplist key &optional default)
  "Like getf but using #'equal for testing of the property key.

@Arguments
proplist - Property list
key - Lisp Object ervong as key in property list.

@Example
(get-prop '(\"George\" \"Maciunas\" \"Simone\" \"de Beauvoir\") \"Simone\") ; => \"de Beauvoir\"

@See-also
delete-props
do-proplist
do-proplist/collecting
get-props-list
map-proplist
with-props
"

  (or (second (member key proplist :test #'equal)) default))


(defun n-lin (x min max)
  "Return the linear interpolation for a normalized value in the range
/[min..max]/ as a float value.

@Arguments
x - An input value in the range /[0..1]/ to be interpolated.
min - The output value for /x = 0/.
max - The output value for /x = 1/.
@Examples
#+BEGIN_SRC lisp
(n-lin 0 10 20) ; => 10.0

(n-lin 0.5 10 20) ; => 15.0

(n-lin 1 10 20)  ; => 20.0
#+END_SRC

@See-also
exp-n
lin-n
m-exp
m-lin
n-exp
n-exp-dev
n-lin-dev
"
  (float (+ min (* (- max min) x))))

;;; (n-lin 0 10 1000) -> 10
;;; (n-lin 0.5 10 1000) -> 505.0
;;; (n-lin 1 10 1000) -> 1000

(defun lin-n (val min max)
  "Return the reverse linear interpolation for a value in the range
/[min..max]/ as a normalized float value.

@Arguments
x - An input value in the range /[min..max]/ to be interpolated.
min - The minimum value.
max - The maximum value.

@Examples

(lin-n 10 10 20) ; => 0.0

(lin-n 15 10 20) ; => 0.5

(lin-n 20 10 20) ; => 1.0

@See-also
exp-n
m-exp
m-lin
n-exp
n-exp-dev
n-lin
n-lin-dev
"
  (float (/ (- val min) (- max min)) 1.0))

(defun n-exp (x min max)
  "Return the exponential interpolation for a normalized value in the
range /[min..max]/ as a float value.

 @Arguments
x - An input value in the range /[0..1]/ to be interpolated.
min - The output value for /x = 0/.
max - The output value for /x = 1/.
@Examples
#+BEGIN_SRC lisp
(n-exp 0 1 100) ; => 1.0

(n-exp 0.5 1 100) ; => 10.0

(n-exp 1 1 100) ; => 100.0
#+END_SRC

@See-also
exp-n
lin-n
m-exp
m-lin
n-exp-dev
n-lin
n-lin-dev
"
  (float (* min (expt (/ max min) x))))

;;; (n-exp 0 10 1000) -> 10
;;; (n-exp 0.5 10 1000) -> 100.0
;;; (n-exp 1 10 1000) -> 1000

(defmacro exp-n (val min max)
  "Return the reverse exponential interpolation for a value in the range
/[min..max]/ as a normalized float value. /Min/ and /max/ have to be
positive numbers.

@Arguments
x - An input value in the range /[min..max]/ to be interpolated.
min - The minimum value.
max - The maximum value.
@Examples

(exp-n 1 1 100) ; => 0.0

(exp-n 10 1 100) ; => 0.5

(exp-n 100 1 100) ; => 1.0

@See-also
lin-n
m-exp
m-lin
n-exp
n-exp-dev
n-lin
n-lin-dev
"
  (let ((quot (if (zerop min) 0 (/ max min))))
    `(log ,(/ val min) ,quot)))

(defun m-exp (x min max)
  "Return the exponential interpolation for a MIDI value in the range
/[min..max]/ as a float value. The min and max values have to be
positive.

@Arguments
x - An input value in the range /[0..127]/ to be interpolated.
min - The output value for /x = 0/.
max - The output value for /x = 127/.
@Examples
#+BEGIN_SRC lisp
(m-exp 0 1 100) ; => 1.0 (100.0%)

(m-exp 64 1 100) ; => 10.18296

(m-exp 127 1 100) ; => 100.0
#+END_SRC

@See-also
exp-n
lin-n
m-lin
n-exp
n-exp-dev
n-lin
n-lin-dev
"
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
  "Return the linear interpolation for a MIDI value in the range
/[min..max]/ as a float value.

@Arguments
x - An input value in the range /[0..127]/ to be interpolated.
min - The output value for /x = 0/.
max - The output value for /x = 127/.
@Examples
#+BEGIN_SRC lisp
(m-lin 0 10 20) ; => 10.0

(m-lin 64 10 20) ; => 15.039371

(m-lin 127 10 20)  ; => 20.0
#+END_SRC

@See-also
exp-n
lin-n
m-exp
n-exp
n-exp-dev
n-lin
n-lin-dev
"
  (float (+ min (* (- max min) (/ x 127))) 1.0))

(defun ntom (n)
  "Return rounded MIDI value mapped from normalized n in the range
[0..1].

@Arguments
n - Number in the range [0..1]

@Examples
(ntom 0) ; => 0

(ntom 0.1) ; => 13

(ntom 0.5) ; => 64

(ntom 1) ; => 127

@See-also
mton
"
  (round (* n 127)))

(defun mton (m)
  "Return normalized value mapped from MIDI value m in the range
[0..127] as a single float.

@Arguments
m - Number in the range [0..127]

@Examples
(mton 0) ; => 0

(mton 13) ; => 0.10236221

(mton 63.5) ; => 0.5

(mton 127) ; => 1.0

@See-also
ntom
"
  (float (/ m 127) 1.0))

;;; (n-lin 0 10 1000) -> 10
;;; (n-lin 0.5 10 1000) -> 505.0
;;; (n-lin 1 10 1000) -> 1000

(defun mcn-lin (x min max)
  "Linear interpolation for midivalues (x = [0..127])"
  (n-lin (/ x 127) min max))

(defun mcn-exp (x min max)
  "Exponential interpolation for midivalues (x = [0..127])"
  (n-exp (/ x 127) min max))

(defun r-exp (min max)
  "Random value between [min..max] with exponential distribution.

@Arguments
min - Number indicationg the minimum value.
max - Number indicationg the maximum value.

@See-also
r-lin
"
  (* min (expt (/ max min) (random 1.0))))

(defun r-lin (min max)
  "Random value between [min..max] with linear distribution.

@Arguments
min - Number indicationg the minimum value.
max - Number indicationg the maximum value.

@See-also
r-exp
"
  (+ min (* (- max min) (random 1.0))))

(defun rand (max)
  "Random value between [0..max-1] with linear distribution."
  (r-lin 0 max))

(defun n-exp-dev (x max)
  "Return a random deviation factor, the deviation being exponentially
interpolated between /1/ for /x = 0/ and /[1/max..max]/ for /x = 1/.

@Arguments
x - An input value in the range /[0..1]/ to be interpolated.
max - The maximum deviation factor for /x = 1/;
@Examples
#+BEGIN_SRC lisp
(n-exp-dev 0 4) ; => 1.0

(n-exp-dev 0.5 4) ; a random value exponentially distributed in the range [0.5..2.0]

(n-exp-dev 1 4) ; a random value exponentially distributed in the range [0.25..4.0]
#+END_SRC

@See-also
exp-n
lin-n
m-exp
m-lin
n-exp
n-lin
n-lin-dev
"
  (r-exp
   (n-exp x 1 (/ max))
   (n-exp x 1 max)))

(defun r-exp-dev (max)
  "return a random deviation factor, the deviation being exponentially
interpolated between 1 for x=0 and [1/max..max] for x=1."
  (n-exp-dev (random 1.0) max))

(defun n-lin-dev (x max)
  "Return a random deviation value, the deviation being linearly
interpolated between /0/ for /x = 0/ and /[-max..max]/ for /x = 1/.

@Arguments
x - An input value in the range /[0..1]/ to be interpolated.
max - The maximum deviation value for /x = 1/;
@Examples
#+BEGIN_SRC lisp
(n-lin-dev 0 4) ; => 0

(n-lin-dev 0.5 4) ; a random value linearly distributed in the range [-2.0..2.0]

(n-lin-dev 1 4) ; a random value linearly distributed in the range [-4.0..4.0]
#+END_SRC

@See-also
exp-n
lin-n
m-exp
m-lin
n-exp
n-exp-dev
n-lin
"
  (if (zerop x)
      0
      (float (* max (- x (random (* 2.0 x)))) 1.0)))

(defun r-lin-dev (max)
  "return a random deviation factor, the deviation being linearly
interpolated between 0 for x=0 and [-max..max] for x=1."
  (n-lin-dev (random 1.0) max))

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
  "Return closure with ipfn bound to a linear interpolation of the
input range 0..127 between min and max."
  `(let ((ipfn (lambda (x) (m-lin ,min ,max))))
     (lambda (d2)
       (when (numberp d2)
         ,@body))))

(defmacro with-exp-midi-fn ((min max) &body body)
  "return closure with ipfn bound to an exponential interpolation of
the input range 0..127 between min and max."
  `(let ((ipfn (lambda (x) (m-exp ,min ,max))))
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
  "Like incf but multiplying instead of adding.

@Arguments
place - A setfable place.
number - Number indicating the multiplication factor.

@Examples
(defvar *test* 2) ; => *test*

(multf *test* 3) ; => 6

*test*  ; => 6
"

  )

#|
(defmacro rmprop (plist key &optional default)
  "like getf, but removing the property from the plist."
  `(prog1
       (getf ,plist ,key ,default)
     (remf ,plist ,key)))
|#

(defun array-slice (arr row-idx)
  "Return the row with index /row-idx/ of a 2-dimensional array as
1-dimensional array, sharing the same data structure by utilizing
Common Lisp's displaced array functionality.

@Arguments
arr - 2-dimensional Array.
row-idx - Non Negative Integer denoting the Index of the row to return.
"
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
      :displaced-index-offset (* row-idx (array-dimension arr 1))))

;;; from cm:

(defun cd (&optional (dirarg (user-homedir-pathname)))
  "Change the current working directory to /dirarg/ or to $HOME if dirarg
is not supplied.

@Arguments
dirarg - String or Pathname.

@See-also
pwd
"
  (let ((dir (if (stringp dirarg)
                 (string-right-trim '(#\/) dirarg)
                 dirarg)))
    (uiop:chdir dir)
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
  "Return the current working directory as a pathname.

@See-also
cd
"
  (uiop:getcwd))

(defmacro defconst (symbol value)
 `(defconstant ,symbol 
    (or (and (boundp ',symbol) 
             (symbol-value ',symbol))
        ,value)))

(defun r-elt (seq)
  "Return a random element of seq.

@Arguments
seq - a sequence fulfilling the predicate /(typep seq 'sequence)/
like a list or a vector.

@Examples
(r-elt #(1 2 3 4)) ; => 1, 2, 3 or 4

(r-elt '(dog cat bird cow)) ; => dog, cat, bird or cow
"
  (elt seq (random (length seq))))

(defun getf-or-elt (seq id)
  "depending on the type of id retrieve the element with the #'elt
function or with getf."
  (cond
    ((numberp id) (elt seq id))
    (t (getf seq id))))

(defun r-getf (list &rest props)
  "Recursively traverse nested /list/ using /props/ as idx. The values for
props can be either keywords/symbols (using #'getf) or numbers (using
#'elt).

@Arguments
list - a nested List to search.
props - one or more Keywords/Symbols, or Numbers interpreted as idx.

@Examples

(defvar *geodata*
  '(:Italy
    (:Latium (:Rome (:Inhabitants 2749031 :size 1287.36 :River \"Tevere\"))
     :Lombardy (:Milano (:Inhabitants 1349930 :size 182 :River \"Naviglio Grande\")))
    :Kenia
    (:Nairobi (:Nairobi (:Inhabitants 4397073 :size 703.9 :River \"Athi\")))
    :Germany
    (:Bavaria (:Munich (:Inhabitants 1510378 :size 310.7 :River \"Isar\"))
     :Berlin (:Berlin (:Inhabitants 6340918 :size 891.7 :River \"Spree\")))))

(r-getf *geodata* :Kenia :Nairobi :Nairobi :Inhabitants) ; => 4397073

(r-getf *geodata* :Germany)

;; => (:bavaria (:munich (:inhabitants 1510378 :size 310.7 :river \"Isar\"))
;;     :berlin (:berlin (:inhabitants 6340918 :size 891.7 :river \"Spree\")))

(r-getf *geodata* :Italy 3 :Rome)

;; => (:inhabitants 2749031 :size 1287.36 :river \"Tevere\")

(r-getf *geodata* 5 3)
;; => (:berlin (:inhabitants 6340918 :size 891.7 :river \"Spree\"))

(r-getf *geodata* :Italy 1 1 5) ; => \"Naviglio Grande\"
"
  (reduce #'getf-or-elt props :initial-value list))

(defun index-list (list &key (n 0))
  "Return /list/ with increasing indexes consed to the front of each
element of list starting from /n/.

@Arguments
list - List containing elements which get prepended indices.
n - Integer denoting starting index

@Example
(index-list '(a b c d e)) ; => ((0 . a) (1 . b) (2 . c) (3 . d) (4 . e))
"
  (if (null list)
      nil
      (cons (cons n (first list)) (index-seq (rest list) (1+ n)))))

(defun subseqx (seq start &optional end)
  "Like #'subseq, but allowing negative values for /end/, indicating the
number of elems at the end of /seq/ to be omitted.

@Arguments
seq - A Common Lisp Sequence.
start - Non Negative Integer denoting starting index of seq.
end - Integer denoting last element's position in seq. If positive, the last element is /(elt seq (1- end))/. If end is negative, /(abs end)/ denotes the number of elements to be omitted from the end of seq.

@Examples
(subseqx '(a b c d e f g) 0 3) ; => (a b c)

(subseqx '(a b c d e f g) 0 -2) ; => (a b c d e)
"
  (let ((end (and end (if (< end 0) (+ end (length seq)) end))))
    (subseq seq start end)))


(defun port-available-p (portno)
  "Check if IP port is available on /localhost/ by issuing shell
command. Only works on Unix with the /lsof/ program installed.

@Arguments
portno - Integer in the range [0..65535]
"
  (string= ""
           (string-trim '(#\NEWLINE)
                        (with-output-to-string (out)
                          (uiop::run-program (format nil "lsof -i:~d" portno)
                                             :ignore-error-status t
                                             :output out)))))

(defmacro defparameter* (&rest pairs)
  "Form for the definition of multiple parameters. /Pairs/ are one or
more elements, being the arguments of a single defparameter form.

@Arguments

pairs - one or more elements, either a Symbol or a list of 1-3
elements with a Symbol as first and a String as third element.

@Examples

(defparameter* *a* (*b*) (*c* 1) (*d* 1 \"parameter d\"))

;; The above form is the same as:

(progn
  (defparameter *a* nil)
  (defparameter *b* nil)
  (defparameter *c* 1)
  (defparameter *d* \"parameter d\"))
@See-also
defvar*
"
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
  "Form for the definition of multiple variable. /Pairs/ are one or
more elements, being the arguments of a single defparameter form.

@Arguments

pairs - one or more elements, either a Symbol or a list of 1-3
elements with a Symbol as first and a String as third element.

@Examples

(defvar​* *​a​* (*​b​*) (*​c​* 1) (*​d​* 1 \"variable d\"))

;; The above form is the same as:

(progn
  (defvar *a* nil)
  (defvar *b* nil)
  (defvar *c* 1)
  (defvar *d* \"variable d\"))
@See-also
defparameter*
"  `(progn
     ,@(loop for entry in pairs
             collect (cond
                       ((symbolp entry)
                        `(defvar ,entry nil))
                       ((third entry)
                        `(defvar ,(first entry) ,(second entry) ,(third entry)))
                       (t
                        `(defvar ,(first entry) ,(second entry)))))))

(defun bool (pred)
  "Enforce /t/ or /nil/ on pred.

@Arguments
pred - Any lisp expression.

@Examples
(bool 1) ; => t
(bool nil) ; => nil
"
  (if pred t))

(defmacro dolist-db ((args list) &body body)
  "like dolist, but using destructuring-bind of args on each list element."
  (let ((item (gensym)))
    `(dolist (,item ,list)
       (destructuring-bind ,args ,item
         ,@body))))

(defun empty-vector (num)
  "Return a simple-vector of size /num/ filled with nil."
  (make-array num :initial-element nil))
