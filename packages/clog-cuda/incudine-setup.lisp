;;;
;;; incudine-setup.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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
(export '(setup-meters
          setup-io
;;;          input-bus
          node-free-unprotected
;;;          clear-buses
;;;          cp-input-buses
;;;          cp-output-buses
;;;          bus-value
;;;          mix-bus-to-out
;;;          bus-to-out
          )
        :incudine)

(defvar *aux* (incudine.external:foreign-alloc-sample
               (* 256 *number-of-input-bus-channels*)))

(declaim (inline aux))
(defun aux (n)
  (smp-ref *aux* n))

(declaim (inline set-aux))
(defun set-aux (n value)
  (setf (smp-ref *aux* n) (sample value)))

(defsetf aux set-aux)

#|
(define-vug input-bus ((channel fixnum))
  (bus (the fixnum
         (+ (the fixnum
              (* current-frame *number-of-input-bus-channels*))
            channel))))
|#



#|
(dsp! cp-input-buses ((first-input channel-number) (first-bus channel-number)
                      (num-channels channel-number))
  "cp all audio inputs to buses starting at first-in-bus + bus-offset."
  (:defaults 0 0 *number-of-input-bus-channels*)
  (let ((numchans (min num-channels *number-of-input-bus-channels*)))
    (foreach-frame
      (dochannels (current-channel numchans)
        (setf (input-bus (+ current-channel first-bus))
              (audio-in (+ current-channel first-input)))))))

(dsp! cp-output-buses ((first-out-bus channel-number))
  "cp all audio outputs to buses starting at first-out-bus."
  (:defaults 8)
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (setf (input-bus (+ current-channel first-out-bus))
            (audio-out current-channel)))))

(dsp! bus-to-out ((numchannels channel-number) (startidx channel-number))
  (foreach-frame
    (dochannels (current-channel numchannels)
      (setf (audio-out current-channel)
            (input-bus (+ current-channel startidx))))))

(dsp! mix-bus-to-out ((startidx channel-number) (numchannels channel-number))
  (:defaults 8 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (incf (audio-out current-channel) (input-bus (+ current-channel startidx))))))

(dsp! clear-buses ((startidx channel-number) (numchannels channel-number))
  (:defaults 16 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (setf (input-bus (+ current-channel startidx)) +sample-zero+))))
|#

(defun setup-io ()
  (free 0)
  (sleep 0.1)
  (make-group 100)
  (make-group 200 :after 100)
  (make-group 300 :after 200)
  (make-group 400 :after 300)
;;  (clear-buses 0 32 :id 1 :head 100)
;;  (cp-input-buses :id 2 :tail 100)
;;  (mix-bus-to-out :id 3 :startidx 0 :head 300)
;;  (cp-output-buses :id 4 :tail 300)
  )

(defun node-free-unprotected ()
  "Free all Incudine nodes of /group 200/. For details of the function of
this group refer to section
<<clamps:General Incudine Setup>> in Clamps Packages.

@See-also
rts-hush
"
  (dogroup (n (node 200))
    (free n)))

;;; (setup-io)
;;; (dump (node 0))
;;; (block-size)
;;; (set-rt-block-size 256)
;;; (rt-start)

#|
(define-vug input-bus ((channel fixnum))
  (bus (the fixnum
         (+ (the fixnum
              (* current-frame *number-of-input-bus-channels*))
            channel))))

(dsp! cp-input-buses ()
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (setf (input-bus current-channel)
            (audio-in current-channel)))))

(dsp! out-test ()
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (cout (input-bus current-channel)))))
|#

;;; (member :slynk *features*)
;;; (member :slime *features*)
