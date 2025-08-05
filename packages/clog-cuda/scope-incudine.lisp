;;; 
;;; scope-incudine.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package #:incudine)
(export '(scope-dsp) :incudine)

(define-vug loop-counter (max)
  (with-samples
      ((phase -1.0d0))
    (prog1
        (incf phase)
      (cond ((>= phase (- max 1.0d0)) (setf phase -1.0d0))))))

#|
(define-vug scope-vug ((in channel-number) freq (ref cl-refs:ref-object))
  (:defaults 0 10 nil)
  (with ((size (round-sample (/ *sample-rate* freq)))
         (bufsize 100)
         (buffer (make-buffer bufsize)))
    (declare (alexandria:non-negative-fixnum size bufsize))
    (foreach-frame
      (let ((idx (round-sample (loop-counter size))))
        (incudine.util:nrt-msg :info "~a ~a" idx current-frame)
        (cond
          ((= idx 1) (incudine.util:nrt-msg :info "bingo ~a" idx)
           (cl-refs:set-val ref buffer))
          ((< idx bufsize) (set-buffer-value buffer idx (audio-out in current-frame))))))))
|#

(defun copy-buffers (src dest)
  "Return a copy of BUFFER."
  (declare (type buffer src dest))
  (if (free-p src)
      (incudine-error "The buffer is unusable.")
      (progn
        (foreign-copy-samples (buffer-data dest) (buffer-data src)
                              (buffer-size src))
;;;      (copy-struct-slots buffer (file textfile-p) src dest)
        dest)))

(define-vug scope-vug ((in channel-number) freq (ref cl-refs:ref-object))
  (:defaults 0 10 nil)
  (with ((size (round-sample (/ *sample-rate* freq)))
         (bufsize (incudine:buffer-size (cl-refs:get-val ref)))
         (buffer (make-buffer bufsize)))
    (declare (alexandria:non-negative-fixnum size bufsize)
             (buffer buffer))
    (foreach-frame
      (let ((idx (round-sample (loop-counter size))))
        ;;;(incudine.util:nrt-msg :info "~a ~a" idx current-frame)
        (when (zerop idx)
          ;;          (incudine.util:nrt-msg :info "bingo ~a" idx)
          (let ((gui-buf (cl-refs:get-val ref)))
            (copy-buffers buffer gui-buf)
            (at (now) (lambda () (cl-refs:set-val ref gui-buf :force t)))))
        (when (< idx bufsize) (set-buffer-value buffer idx (audio-out in current-frame)))))))

(define-vug in-scope-vug ((in channel-number) freq (ref cl-refs:ref-object))
  (:defaults 0 10 nil)
  (with ((size (round-sample (/ *sample-rate* freq)))
         (bufsize (incudine:buffer-size (cl-refs:get-val ref)))
         (buffer (make-buffer bufsize)))
    (declare (alexandria:non-negative-fixnum size bufsize)
             (buffer buffer))
    (foreach-frame
      (let ((idx (round-sample (loop-counter size))))
        ;;;(incudine.util:nrt-msg :info "~a ~a" idx current-frame)
        (when (zerop idx)
          ;;          (incudine.util:nrt-msg :info "bingo ~a" idx)
          (let ((gui-buf (cl-refs:get-val ref)))
            (copy-buffers buffer gui-buf)
            (at (now) (lambda () (cl-refs:set-val ref gui-buf :force t)))))
        (when (< idx bufsize) (set-buffer-value buffer idx (audio-in in current-frame)))))))

(define-vug out-scope-vug ((out channel-number) freq (ref cl-refs:ref-object))
  (:defaults 0 10 nil)
  (with ((size (round-sample (/ *sample-rate* freq)))
         (bufsize (incudine:buffer-size (cl-refs:get-val ref)))
         (buffer (make-buffer bufsize)))
    (declare (alexandria:non-negative-fixnum size bufsize)
             (buffer buffer))
    (foreach-frame
      (let ((idx (round-sample (loop-counter size))))
        ;;;(incudine.util:nrt-msg :info "~a ~a" idx current-frame)
        (when (zerop idx)
          ;; (nrt-funcall
          ;;  (lambda ()
          ;;    (let ((gui-buf (cl-refs:get-val ref)))
          ;;      (copy-buffers buffer gui-buf)
          ;;      (cl-refs:set-val ref gui-buf :force t))))
          )
        (when (< idx bufsize) (set-buffer-value buffer idx (audio-out out current-frame)))))))

(define-vug bus-scope-vug ((chan channel-number) freq (ref cl-refs:ref-object))
  (:defaults 0 10 nil)
  (with ((size (round-sample (/ *sample-rate* freq)))
         (bufsize (incudine:buffer-size (cl-refs:get-val ref)))
         (buffer (make-buffer bufsize)))
    (declare (non-negative-fixnum size bufsize)
             (buffer buffer))
    (foreach-frame
      (let ((idx (round-sample (loop-counter size))))
        (declare (type non-negative-fixnum idx))
        ;;;(incudine.util:nrt-msg :info "~a ~a" idx current-frame)
        (when (zerop idx)
          ;;          (incudine.util:nrt-msg :info "bingo ~a" idx)
          (reduce-warnings
            (nrt-funcall
             (lambda ()
               (let ((gui-buf (cl-refs:get-val ref)))
                 (copy-buffers buffer gui-buf)
                 (cl-refs:set-val ref gui-buf :force t))))))
        (when (< idx bufsize) (set-buffer-value (the buffer buffer)
                                                (the non-negative-fixnum idx)
                                                (incudine::audio-bus chan current-frame)))))))

(dsp! in-scope ((chan channel-number) freq (ref cl-refs:ref-object))
   (:defaults 0 nil (cl-refs:make-ref (make-buffer 100)))
  (foreach-frame (in-scope-vug chan freq ref)))

(dsp! out-scope ((chan channel-number) freq (ref cl-refs:ref-object))
   (:defaults 0 nil (cl-refs:make-ref (make-buffer 100)))
  (foreach-frame (out-scope-vug chan freq ref)))

(dsp! bus-scope ((chan channel-number) freq (ref cl-refs:ref-object))
   (:defaults 0 nil (cl-refs:make-ref (make-buffer 100)))
  (foreach-frame (bus-scope-vug chan freq ref)))

#|
(defun scope-dsp (&key (group 300) (num 1)
                    id-callback refs (freq 5) (audio-bus 0))
  (loop
    for idx below num
    do (progn
         (scope (+ audio-bus idx) freq (aref refs idx)
                :action (lambda (n)
                          (funcall id-callback (node-id n)))
                :tail group))))
|#
