;;;; cm-utils.lisp
;;; (ql:quickload "cm-utils")
(in-package #:cm-utils)

(rt-start)
(midi-open-default :direction :output)
(setf cm::*rts-out* (new cm::incudine-stream))

(defun test-proc ()
  (rt-proc
    (loop
       for x below 30
       do (progn
            (output (new midi :keynum 60 :time (now) :duration 0.05))
            (rt-wait 0.2)))))

(defun test-proc2 ()
  (rt-proc
    (progn
      (output (new midi :keynum 60 :time (now) :duration 0.05))
      (rt-wait 0.2)
      (output (new midi :keynum 60 :time (now) :duration 0.05))
      (rt-wait 0.1))))

(sprout (test-proc))

(events (test-proc)
        "/tmp/test.midi")

(defun test-proc ()
  (rt-proc
    (loop
       for x below 30
       do (progn
            (output (new midi :keynum 60 :time (now) :duration 0.05))
            (rt-wait 0.2)
            (output (new midi :keynum 60 :time (now) :duration 0.05))
            (rt-wait 0.1)))))

(defun test-proc2 ()
  (rt-proc
    (loop
       for x below 4
       do (progn
            (format t "~&test-proc2-1~%")
            (rt-wait 0.5)
            (format t "~&test-proc2-2~%")
            (rt-wait 0.5)))))

(defun test-proc3 ()
  (rt-proc
    (loop
       for x below 4
       do (progn
            (format t "~&test-proc3-1~%")
            (rt-wait 0.5)
            (rt-sub (test-proc2))
            (format t "~&test-proc3-2~%")
            (rt-wait 0.5)))))

(sprout (test-proc2))
(sprout (test-proc3))

(events (test-proc) "/tmp/test.midi" :play nil)

(defun test-proc4 ()
  (rt-proc
    (loop
       for x below 4
       do (progn
            (format t "~&test-proc4-1~%")
            (rt-wait 0.5)
            (format t "~&test-proc4-2~%")
            (rt-wait 1)
            ))))

(defun test-proc5 ()
  (rt-proc
    (loop
       for x below 2
       do (progn
            (format t "~&test-proc5-1~%")
            (rt-wait 1.5)
            (subproc (test-proc4))
            (format t "~&test-proc5-2~%")
            (rt-wait 2)
            ))))

(defparameter *test* nil)

(setf *test* (test-proc5))

(funcall *test*)


(rt-proc
    (loop
       for x below 2
       do (progn
            (format t "~&test-proc5-1~%")
            (yield t)
            (subproc (test-proc4))
            (format t "~&test-proc5-2~%~%")
            (yield t)
            )))

(setf *test*
      (rt-proc
        (subproc (test-proc4))))

(funcall (test-proc4))
(funcall *test*)

(loop
   with x = 0
   while (< x 40)
   do (incf x)
   finally (return x))

(untrace)
(sprout (test-proc5))


(defun test-proc4 (num)
  (rt-proc
    (loop
       for x below num
       do (progn
            (format t "~&test-proc4-1~%")
            (rt-wait 0.5)
            (format t "~&test-proc4-2~%")
            (rt-wait 1)
            ))))


(defun test-proc5 (num)
  (rt-proc
    (loop
       for x below num
       do (progn
            (format t "~&test-proc5-1~%")
            (rt-wait 2)
            (rt-sub (test-proc4 num))
            (format t "~&test-proc5-2~%~%~%")
            (rt-wait 1.5)
            ))))

(sprout (test-proc4 1))

(sprout (test-proc5 3))


(sprout
 (rt-proc
   (dotimes (i 4)
     (progn
       (output (new midi :time (now) :keynum 63))
       (rt-wait 0.5)
       (if (evenp i)
           (loop
              for i below 3
              do (progn
                   (output (new midi :time (now) :keynum 61))
                   (rt-wait 1.3))))
       (output (new midi :time (now) :keynum 60))
       (rt-wait 1)))))

(sprout
 (rt-proc
   (dotimes (i 4)
     (output (new midi :time (now) :keynum 63))
     (rt-wait 0.5)
     (if (evenp i)
         (dotimes (n 3)
           (output (new midi :time (now) :keynum 61))
           (rt-wait 1.3)))
     (output (new midi :time (now) :keynum 60))
     (rt-wait 1))))

(defun 4-notes ()
  (rt-proc
    (dotimes (i 4)
      (output (new midi :time (now) :keynum 61))
      (rt-wait 0.125))))

(sprout (4-notes))

(sprout
 (rt-proc
   (dotimes (i 4)
     (output (new midi :time (now) :keynum 63))
     (rt-wait 0.5)
     (if (evenp i)
         (rt-sub (4-notes))
         (rt-sprout (4-notes)))
     (output (new midi :time (now) :keynum 60))
     (rt-wait 1))))

(rt-sprout (4-notes))

(events
 (rt-proc
   (dotimes (i 4)
     (output (new midi :time (now) :keynum 63))
     (rt-wait 0.5)
     (if (evenp i)
         (rt-sub (4-notes))
         (rt-sprout (4-notes)))
     (output (new midi :time (now) :keynum 60))
     (rt-wait 1)))
 "/tmp/test.midi")

(defun arpeggio (chd dir)
  (rt-proc
    (let ((*local-time* 0))
      (loop
         for pitch in
           (case dir
             (up (sort (copy-list chd) #'<))
             (t (sort (copy-list chd) #'>)))
         do (progn
              (output (new midi :time (now) :keynum pitch))
              (rt-wait 0.09))))))

(setf *local-time* 0)
(let ((*local-time* 0))
  (sprout (arpeggio '(60 62 65 71) 'up)))
(sprout (arpeggio '(60 62 65 71) 'down))

(defun rasgueado (chd dirs dur)
  (let ((*local-time* 0))
    (rt-proc
      (loop
         until (>= *local-time* dur)
         do (progn
              (format t "~a~%" *local-time*)
              (loop
                 for dir in dirs
                 until (>= *local-time* dur)
                 do (rt-sub (arpeggio chd dir)))
              (format t "~&local-time-b: ~a" *local-time*))))))

(sprout
 (rt-proc
   (let ((chd '(60 62 65 71))
         (dirs '(up up up down)))
     (loop
        for dir in dirs
        do (rt-sub (arpeggio chd dir))))))

(sprout (rasgueado '(60 62 65 71) '(up up up down) 3))

(setf *local-time* 0)

(now)


