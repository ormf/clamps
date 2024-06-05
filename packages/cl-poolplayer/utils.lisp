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

(defmacro r-elt (place) `(elt ,place (random (length ,place))))

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



#|
(let ((seq '(:x 10 :y 3)))
  (multf (getf seq :y) 2)
  seq)

|#



