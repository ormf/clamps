;;; 
;;; utils.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-poolplayer)

(defparameter *buffers* (make-hash-table :test #'equal))
(defparameter *buffer-idxs* (make-hash-table :test #'equal))

(defmacro vector-extend (v1 v2)
  `(setf ,v1 (concatenate 'vector ,v1 ,v2)))

;;; (defmacro r-elt (seq) `(elt ,seq (random (length ,seq))))

(defmacro set-sv (p slot val)
  `(setf (sv (aref *players* ,p) ,slot) ,val))

(defmacro set-env ((o s) &body args)
  (let ((env (gensym)))
    `(let ((,env (sv ,o ,s)))
       (sv ,env ,@args)
       ,env)))

(defun keyword->symbol (keyword)
  (let ((name (symbol-name keyword)))
    (or (find-symbol name)
        (intern name :cl-poolplayer))))

(defmacro player-set (p &body args)
  `(sv (aref *players* ,p) ,@args))

(defmacro toggle (p)
  `(setf ,p (if ,p nil t)))

#|
(defun get-dtime-fn (mina maxa minb maxb &key (distribution '(0 0 0 0 1 2 3)) (thresh 0.5))
  "return a function calculating a delta-time on each call. The
distribution specifies a random distribution (using cm's weighting) to
determine the number of dtimes returned between [mina..maxa] before
returning a number between [minb..maxb]. An :thresh keyword determines
a threshold for [minb..maxb], below which no [mina..maxa] values are
returned."
  (let* ((rep-stream (new weighting :of distribution))
         (curr-rep (next rep-stream))
         (bmax (max minb maxb)))
    (lambda (x)
      x
      (decf curr-rep)
      (if (or (<= bmax thresh) (minusp curr-rep))
          (progn
            (setf curr-rep (next rep-stream))
            (r-exp minb maxb))
          (r-exp mina maxa)))))
|#

(defun get-dtime-fn (mina maxa minbfn maxbfn &key (distribution '((0 :weight 9) (1 :weight 3) 2 3 4)) (thresh 0.5))
  "return a function calculating a delta-time on each call. The
distribution specifies a random distribution (using cm's weighting) to
determine the number of dtimes returned between [mina..maxa] before
returning a number between calling [minbfn..maxbfn] on x. A :thresh
keyword determines a threshold for [minb..maxb], below which no
[mina..maxa] values are returned."
  (let* ((rep-stream (new weighting :of distribution))
         (curr-rep (next rep-stream)))
    (lambda (x &rest args)
      (declare (ignorable args))
      (let* ((minb (if (numberp minbfn) minbfn (funcall minbfn x)))
             (maxb (if (numberp maxbfn) maxbfn (funcall maxbfn x))))
        (decf curr-rep)
        (if (or (<= (max minb maxb) thresh) (minusp curr-rep))
            (progn
              (setf curr-rep (next rep-stream))
              (r-exp minb maxb))
            (r-exp mina maxa))))))



(defun get-dtime-fn-no-x (mina maxa minb maxb &key (distribution '(1 1 1 1 2 3 4)))
  "return a function calculating a delta-time on each call. The
distribution specifies a random distribution (using cm's weighting) to
determine the number of dtimes returned between [mina..maxa] before
returning a number between [minb..maxb]."
  (let* ((rep-stream (new weighting :of distribution))
         (curr-rep (next rep-stream)))
    (lambda ()
      (decf curr-rep)
      (if (zerop curr-rep)
          (progn
            (setf curr-rep (next rep-stream))
            (r-exp minb maxb))
          (r-exp mina maxa)))))

(defun zerofn (x)
  (declare (ignore x))
  0)

(defun fnzero ()
  (lambda () 0))

(defun valuefn (val)
  (lambda (x)
    (declare (ignore x))
    val))



(defun rek-traverse (seq num)
  (if (> num 0) (append seq (rek-traverse (drunk-traverse seq) (1- num)))))

;;; (rek-traverse  '(1 8 4 2 7 3 0 6 5) 150)

(defun marginclip (num limit margin)
  "round num up to limit if less than margin smaller, otherwise clip
num at limit."
  (if (>= num (- limit margin)) limit num))

(defun audio-test (&optional (repeat 14))
  (let ((out 0)
        (params
          (list :buffer (aref *buffers* 4) :pan 0
                :amp *master-amp-db* :out2 0)))
    (labels ((recurse-fn (time)
               (distributed-play
                (append (list :out1
                              (pan-at-idx (setf out (mod (+ out 1) 13)) *circle-cw*))
                        params))
               (if (> (decf repeat) 0)
                   (at (+ time 0.5) #'recurse-fn (+ time 0.5)))))
      (if (> repeat 0)
          (recurse-fn (now))))))

(defmacro digest-interp-form (form &key (round t))
  "Return an interpolation breakpoint list for the [[dict:interp][interp]] function with
alternating min and max values of the form /(x1 min1 x2 max1 x3 min2 x4 max2...)/

x values in the result are going from 0..1, the distance between
succesive x vals calculated by the :dtime function form.

A form /(:dtime 0.1)/  will result in x values /(0 0.1 0.2 0.3 ... 1)/.

A form /(:dtime (n-lin x 0.2 0.5))/ will result in x values /(0 0.1 0.2 0.3 ... 1)/.

 /form/ should contain a =:dtime=, =:min= and =:max= property. The
forms of =:dtime=, =:min= and =:max= can use /x/, referring to its
value and the forms of =:min= and =:max= can additionally use /ip-num/
and /ip-idx/, referring to the current idx and the total num of values
in the interpolated list. =:bindings= is an optional binding form for
variables usable within the context of the =:min= and =:max= forms.

@Arguments
proplist - Property List containing =:dtime=, =:bindings=, =:min= and =:max= properties.
:round - whether the results should get rounded to two decimal places (default t).

@Examples
(digest-interp-form
 (:dtime (n-exp x 0.3 0.05)
  :min (n-lin x 0.5 0)
  :max (n-lin x 0.5 1)))

;; => (0 0.5 0.3 0.65 0.48 0.26 0.61 0.8 0.71 0.15 0.79 0.9 0.86 0.07
;;     0.92 0.96 0.98 0.01 1 1.0)

(plot-2d
 (digest-interp-form
  (:dtime (n-exp x 0.3 0.05)
   :min (n-lin x 0.5 0)
   :max (n-lin x 0.5 1))))

@Image
digest-interp-form-01.png

40

@Example-noheader
(digest-interp-form
  (:bindings ((min 0) (max 1))
   :dtime (n-exp x 0.3 0.05)
   :min (float (* ip-idx (/ 0.5 (1- ip-num))) 1.0)
   :max (- 1.0 (* ip-idx (/ 0.5 (1- ip-num))))))

;; => (0 0.0 0.3 0.94 0.48 0.11 0.61 0.83 0.71 0.22 0.79 0.72 0.86
;;     0.33 0.92 0.61 0.98 0.44 1 0.5)

(plot-2d
 (digest-interp-form
   (:bindings ((min 0) (max 1))
    :dtime (n-exp x 0.01 0.3)
    :min (float (* ip-idx (/ 0.5 (1- ip-num))) 1.0)
    :max (- 1.0 (* ip-idx (/ 0.5 (1- ip-num)))))))

@Image
digest-interp-form-02.png

40

@See-also

get-interp-vals
"

  `(let* ((dtime-fn (lambda (x) ,(getf form :dtime 0.1)))
          (min-fn (lambda (x ip-idx ip-num) (declare (ignorable x ip-idx ip-num)) (let ,(getf form :bindings) ,(getf form :min 0))))
          (max-fn (lambda (x ip-idx ip-num) (declare (ignorable x ip-idx ip-num)) (let ,(getf form :bindings) ,(getf form :max 1))))
          (num-vals 1)
          (x-vals (loop
                    for x = 0 then (if ,round
                                       (dround (+ x (funcall dtime-fn x)))
                                       (+ x (funcall dtime-fn x)))
                    with result
                    while (< x 1)
                    do (progn (push x result)
                              (incf num-vals))
                    finally (return (reverse (push 1 result))))))
     (loop
       for x in x-vals
       for idx from 0
       for y = 0 then (if (zerop y) 1 0)
       append (list x (if ,round
                          (dround (funcall (if (zerop y) min-fn max-fn) x idx num-vals))
                          (funcall (if (zerop y) min-fn max-fn) x idx num-vals))))))

(defun get-interp-vals (proplist)
  "Return an interpolation breakpoint list for the [[dict:interp][interp]] function with
alternating min and max values of the form /(x1 min1 x2 max1 x3 min2 x4 max2...)/

x values in the result are going from 0..1, the distance between
succesive x vals calculated by the :dtime function form.

A form /(:dtime 0.1)/  will result in x values /(0 0.1 0.2 0.3 ... 1)/.

A form /(:dtime (n-lin x 0.2 0.5))/ will result in x values /(0 0.1 0.2 0.3 ... 1)/.

 /form/ should contain a =:dtime=, =:min= and =:max= property. The
forms of =:dtime=, =:min= and =:max= can use /x/, referring to its
value and the forms of =:min= and =:max= can additionally use /ip-num/
and /ip-idx/, referring to the current idx and the total num of values
in the interpolated list. =:bindings= is an optional binding form for
variables usable within the context of the =:min= and =:max= forms.

@Arguments
proplist - Property List containing =:dtime=, =:bindings=, =:min= and =:max= properties.
:round - whether the results should get rounded to two decimal places (default t).

@Examples
(digest-interp-form
 (:dtime (n-exp x 0.3 0.05)
  :min (n-lin x 0.5 0)
  :max (n-lin x 0.5 1)))

;; => (0 0.5 0.3 0.65 0.48 0.26 0.61 0.8 0.71 0.15 0.79 0.9 0.86 0.07
;;     0.92 0.96 0.98 0.01 1 1.0)

(plot-2d
 (digest-interp-form
  (:dtime (n-exp x 0.3 0.05)
   :min (n-lin x 0.5 0)
   :max (n-lin x 0.5 1))))

@Image
digest-interp-form-01.png

40

@Example-noheader
(digest-interp-form
  (:bindings ((min 0) (max 1))
   :dtime (n-exp x 0.3 0.05)
   :min (float (* ip-idx (/ 0.5 (1- ip-num))) 1.0)
   :max (- 1.0 (* ip-idx (/ 0.5 (1- ip-num))))))

;; => (0 0.0 0.3 0.94 0.48 0.11 0.61 0.83 0.71 0.22 0.79 0.72 0.86
;;     0.33 0.92 0.61 0.98 0.44 1 0.5)

(plot-2d
 (digest-interp-form
   (:bindings ((min 0) (max 1))
    :dtime (n-exp x 0.01 0.3)
    :min (float (* ip-idx (/ 0.5 (1- ip-num))) 1.0)
    :max (- 1.0 (* ip-idx (/ 0.5 (1- ip-num)))))))

@Image
digest-interp-form-02.png

40

@See-also

get-interp-vals
"
  (eval `(digest-interp-form ,proplist)))


#|
(get-interp-form
  `(:bindings ((min 0) (max 1))
    :dtime (n-exp x 0.3 0.05)
    :min (float (* idx (/ (1- num))) 1.0)
    :max (- 1.0 (* idx (/ (1- num))))))
|#

