;;; 
;;; midictl.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defparameter *nk2-chan* 4)
(defparameter *player-chans* (vector *nk2-chan* 1 2 3))

(defparameter *player-lookup* (make-hash-table))

(defun init-player-lookup ()
  (loop for chan across *player-chans*
        for name in '(:player1 :player2 :player3 :player4 :nk2)
        for idx from 0
        do (progn
             (setf (gethash idx *player-lookup*) chan)
             (setf (gethash name *player-lookup*) chan))))

(declaim (inline player-chan))
(defun player-chan (idx-or-key) (gethash idx-or-key *player-lookup*))

(defparameter *cc-state* (make-array '(6 128) :element-type 'integer :initial-element 0))
(defparameter *cc-fns* (make-array '(6 128) :element-type 'function :initial-element #'identity))

(defparameter *note-state* (make-array '(16) :element-type 'integer :initial-element 0))
(defparameter *note-fns* (make-array '(16) :element-type 'function :initial-element #'identity))

(defparameter *note-states* ;;; stores last note-on keynum for each player.
  (make-array '(6) :element-type 'integer :initial-element 0))

(declaim (inline last-keynum))
(defun last-keynum (player)
  (aref *note-state* player))

(defun clear-cc-fns (nk2-chan)
  (loop for x below 6
     do (loop for idx below 128
           do (setf (aref *cc-fns* x idx) #'identity)))
  (set-fixed-cc-fns nk2-chan))


(defun set-fixed-cc-fns (nk2-chan)
  (setf (aref *cc-fns* nk2-chan 58)
        (lambda (d2)
          (if (= d2 127)
              (previous-preset))))

  (setf (aref *cc-fns* nk2-chan 59)
        (lambda (d2)
          (if (= d2 127)
              (next-preset))))

  (setf (aref *cc-fns* nk2-chan 46)
        (lambda (d2)
          (if (= d2 127)
              (toggle *midi-debug*))))

  (setf (aref *cc-fns* nk2-chan 41) ;;; start
        (lambda (d2)
          d2
          (stop-all-seqs)
          (start-bo-seqs (now))
          (format t "~&sequences started.")))

  (setf (aref *cc-fns* nk2-chan 42) ;;; stop
        (lambda (d2)
          d2
          (stop-all-seqs)
          (incudine:flush-pending)
          (incudine::node-free-all)
          (format t "~&sequences stopped.")))

  (setf (aref *cc-fns* nk2-chan 0) ;;; stop
        (lambda (d2)
          (setf *master-amp-db*
                (if (zerop d2) -200
                    (+ -40 (* (/ d2 127.0) 52))))
          (format t "~& Master amp (dB): ~5,2f" *master-amp-db*))))



(defun clear-note-fns ()
  (dotimes (n 16)
    (setf (aref *note-fns* n) #'identity)))

(defparameter *midi-debug* nil)

;;; (setf *midi-debug* t)

(defun init-midi ()
  (midi-open-default :direction :input)
  (midi-open-default :direction :output)
  (init-player-lookup)
  (clear-cc-fns *nk2-chan*)
  (set-fixed-cc-fns *nk2-chan*)
  (set-receiver!
     (lambda (st d1 d2)
       (case (status->opcode st)
         (:cc (let ((ch (status->channel st)))
                (progn
                  (if *midi-debug* (format t "~&cc: ~a ~a ~a~%" ch d1 d2))
                  (setf (aref *cc-state* ch d1) d2)
                  (funcall (aref *cc-fns* ch d1) d2)
                  )))))
     *midi-in1*
     :format :raw))


#|
(/ 650 7.14)

(setf (aref *cc-fns* 0 0) #'identity)
(setf (aref *cc-fns* 0 16) #'identity)

(setf (aref *cc-fns* 0 0)
      (lambda (d2) (player-set 0 :dtime (m-exp d2 0.01 4))))

(setf (aref *cc-fns* 0 16)
      (lambda (d2) (player-set 0 :dtime-dev (m-lin d2 0.0 10))))

|#
;;; (aref *cc-state* 0 99)
;;; (setf (aref *note-fns* 0) #'identity)

;;; (cm::stream-receive-stop *midi-in1*)

