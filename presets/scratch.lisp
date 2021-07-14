;;; 
;;; scratch.lisp
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

(in-package :cl-user)

;;; 
;;; presets.lisp
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

(in-package :luftstrom-display)

q(cd "/home/orm/work/kompositionen/luftstrom/lisp/luftstrom/luftstrom-display/")

(defparameter *presets*
  (make-array 100 :initial-element nil))
(defparameter *curr-preset* nil)

(defparameter *presets-file* "presets/schwarm01.lisp")

(defparameter *audio-presets-file* "presets/schwarm01-audio-presets.lisp")

(setf *presets-file* "presets/schwarm-18-11-18.lisp")
(setf *audio-presets-file* "presets/schwarm-audio-presets-18-11-18.lisp")
;;; (load-presets)
;;; (load-audio-presets)

(defparameter *curr-preset-no* 0)
(defparameter *curr-audio-preset-no* 0)

;;; tmp storage for all bound cc-fns in running preset. Used for
;;; suspending current pending actions when changing a preset before
;;; reassignment.

(defparameter *curr-cc-fns* nil)

(defun previous-preset ()
  (let ((next-no (max 0 (1- *curr-preset-no*))))
    (if (/= next-no *curr-preset-no*)
        (progn
          (setf *curr-preset-no* next-no)
          (qt:emit-signal (find-gui :pv1) "setPreset(int)" *curr-preset-no*)
          (edit-preset-in-emacs *curr-preset-no*)))
    *curr-preset-no*))

#|
(next-preset)
(qt:emit-signal (find-gui :pv1) "setPreset(int)" 3)
(previous-preset)
|#

(defun next-preset ()
  (let ((next-no (min 127 (1+ *curr-preset-no*))))
    (if (/= next-no *curr-preset-no*)
        (progn
          (setf *curr-preset-no* next-no)
          (qt:emit-signal (find-gui :pv1) "setPreset(int)" *curr-preset-no*)
          (edit-preset-in-emacs *curr-preset-no*)
          ))
    *curr-preset-no*))

(defun previous-audio-preset ()
  (let ((next-no (max 0 (1- *curr-audio-preset-no*))))
    (if (/= next-no *curr-audio-preset-no*)
        (progn
          (setf *curr-audio-preset-no* next-no)
          (edit-audio-preset-in-emacs *curr-audio-preset-no*)))
    *curr-audio-preset-no*))

(defun next-audio-preset ()
  (let ((next-no (min 127 (1+ *curr-audio-preset-no*))))
    (if (/= next-no *curr-audio-preset-no*)
        (progn
          (setf *curr-audio-preset-no* next-no)
          (edit-audio-preset-in-emacs *curr-audio-preset-no*)))
    *curr-audio-preset-no*))

(defun set-fixed-cc-fns (nk2-chan)
  (setf (aref *cc-fns* nk2-chan 58)
        (lambda (d2)
          (if (= d2 127)
              (previous-preset))))
  (setf (aref *cc-fns* nk2-chan 43)
        (lambda (d2)
          (declare (ignore d2))
          (load-current-preset)))
  (setf (aref *cc-fns* nk2-chan 46)
        (lambda (d2)
          (if (= d2 127)
              (edit-preset-in-emacs *curr-preset-no*))))

  (setf (aref *cc-fns* nk2-chan 59)
        (lambda (d2)
          (if (= d2 127)
              (next-preset))))

  (setf (aref *cc-fns* nk2-chan 45)
        (lambda (d2)
          (if (= d2 127)
              (load-current-preset))))
  
  (setf (aref *cc-fns* nk2-chan 61)
        (lambda (d2)
          (if (= d2 127)
              (previous-audio-preset))))

  (setf (aref *cc-fns* nk2-chan 60)
        (lambda (d2)
          (if (= d2 127)
              (edit-audio-preset-in-emacs *curr-audio-preset-no*))))
  
  (setf (aref *cc-fns* nk2-chan 62)
        (lambda (d2)
          (if (= d2 127)
              (next-audio-preset))))
  (setf (aref *cc-fns* nk2-chan 42)
        (lambda (d2)
          (declare (ignore d2))
          (cl-boids-gpu::reshuffle-life cl-boids-gpu::*win* :regular nil))))

(defun preset->string (preset)
  (format nil "(progn
  (setf *curr-preset*
        (copy-list
         (append
          `(:boid-params
            (~s ~s~&~{~{~s ~s~}~^~%~})
            :audio-args
            (~s ~s~&~{~{~s ~s~}~^~%~})
            :midi-cc-fns
            (~{~{~s ~s~}~^~&~}))
            `(:midi-cc-state ,(alexandria:copy-array *cc-state*)))))
  (load-preset *curr-preset*))"
          (car (getf preset :boid-params))
          (cadr (getf preset :boid-params))
          (loop for (key val) on (cddr (getf preset :boid-params)) by #'cddr collect (list key val))
          (car (getf preset :audio-args))
          (cadr (getf preset :audio-args))
          (loop for (key val) on (cddr (getf preset :audio-args)) by #'cddr collect (list key val))
          (loop for (key val) on (getf preset :midi-cc-fns) by #'cddr collect (list key val))))

;;; (preset->string *curr-preset*)

(defun preset-audio-args (preset)
  (format nil "~&~{~{:~a ~a~}~^~%~}"
          (loop for (key val) on (getf preset :audio-args) by #'cddr collect (list key val))))

;;; (preset-audio-args *curr-preset*)

(defun preset-midi-cc-fns (preset)
  (format nil "~&~{~{~s ~s~}~^~%~}"
          (loop for (key val) on (getf preset :midi-cc-fns) by #'cddr collect (list key val))))

;;; (preset-midi-cc-fns *curr-preset*)


(defun set-value (param val)
  (gui-set-param-value param val)
  (set-param-from-key param val))

;; (set-value :alignmult 3)

;; (set-value :curr-kernel "boids")

(defparameter *emcs-conn* swank::*emacs-connection*)



;;; (clear-obstacles (cl-boids-gpu::systems cl-boids-gpu::*win*))

(defun incudine.scratch::move-test (time num)
  (if (> num 0)
      (let ((next (+ time 10000.3)))
        (format t "~&next: ~a" num)
        (incudine:at next
          #'incudine.scratch::move-test next (decf num)))))

;;; (incudine.scratch::move-test (now) 4)


;;; (move-test (now) 4)



;;; *obstacle-ref*

#|
(%update-system)
(loop
   for v in val
   for ch below 4
   with idx = -1
   do (if v
          (destructuring-bind (type radius) v
            (multiple-value-bind (x y) (keynum->coords (aref *notes* ch))
              (case type
                (0 (new-predator win x y radius))
                (otherwise (new-predator win x y radius))))
            (setf (aref *obstacle-ref* ch) (incf idx)))))

(new-obstacle *win* 100 300 20)
(new-obstacle *win* 300 100 20)
(new-obstacle *win* 500 1000 20)
(new-obstacle *win* 200 430 20)
(new-obstacle *win* 500 150 40)

(new-predator *win* 300 240 20)

(delete-obstacle *win* 1)

|#

(defun digest-boid-param (key val state)
  (case key
    (:num-boids (progn
                  (set-value key *num-boids*)
                  (fudi-send-num-boids *num-boids*)))
    (:obstacles (set-obstacles val state))
    (t (set-value key val))))


(defun get-system-state ()
  (list :num-boids *num-boids*
        :obstacles-state (cl-boids-gpu::get-obstacles-state cl-boids-gpu::*win*)
        :midi-cc-fns (copy-list (getf *curr-preset* :midi-cc-fns))))

;;; (getf (get-system-state) :obstacles-state)

;;; (set-obstacles '((4 25)) (get-system-state))

;(getf (get-system-state) :obstacles)

(defun load-preset (ref &key (presets *presets*))
  (let ((preset (if (numberp ref) (aref presets ref) ref)))
    (if preset
        (let ((state (get-system-state)))
          (deactivate-cc-fns)
          (loop for (key val) on (getf preset :boid-params) by #'cddr
             do (digest-boid-param key val state))
          (gui-set-audio-args (preset-audio-args preset))
          (gui-set-midi-cc-fns (preset-midi-cc-fns preset))
          (clear-cc-fns *nk2-chan*)
          (digest-midi-cc-args (getf preset :midi-cc-fns) (getf preset :midi-cc-state))
          (digest-audio-args (getf preset :audio-args))
          (setf *curr-preset* preset)
          (if (numberp ref) (setf *curr-preset-no* ref))
          (fudi-send-pgm-no ref)))))

(defmacro nk2-ref (ref)
  `(aref *cc-state* *nk2-chan* ,ref))

(defun edit-preset-in-emacs (ref &key (presets *presets*))
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-flock-preset
           ,(progn
              (in-package :luftstrom-display)
              (defparameter swank::*send-counter* 0)
              (preset->string (aref presets ref))) ,(format nil "~a" ref)) t)
        (swank::eval-in-emacs `(edit-flock-preset ,(preset->string ref) ,(format nil "~a" *c urr-preset-no*)) t))))

(defun show-audio-preset (preset-def)
  (view-audio-preset-in-emacs (second (read-from-string preset-def)))
  "nil")

;;; (show-audio-preset '(apr 94))

(defun load-current-preset ()
  (load-preset *curr-preset-no*))

(defun edit-audio-preset-in-emacs (ref)
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-flock-audio-preset
           ,(progn
              (in-package :luftstrom-display)
              (get-audio-preset-load-form ref))
           ,(format nil "~a" ref))
                              t))))

(defun view-audio-preset-in-emacs (ref)
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(view-flock-audio-preset
           ,(progn
              (in-package :luftstrom-display)
              (get-audio-preset-load-form ref))
           ,(format nil "~a" ref))
         t))))


;;; (preset->string (aref *presets* 0))

;;; (load-presets 3)
;;; (load-preset *curr-preset*)

;;; (snapshot-curr-preset)
;;; *num-boids*



(defun capture-preset (preset)
  (setf (getf preset :boid-params)
        (loop for (key val) on (getf preset :boid-params) by #'cddr
           append (capture-param key val)))
  preset)

;;; (clear-cc-fns)

(defun snapshot-curr-preset ()
  (let ((preset (capture-preset *curr-preset*)))
        (progn
          (digest-params preset)
          (loop for (param val) on (getf preset :boid-params) by #'cddr
             do (set-value param val))
          (gui-set-audio-args (preset-audio-args preset))
          (gui-set-midi-cc-fns (preset-midi-cc-fns preset))
          (clear-cc-fns *nk2-chan*)
          (loop for (coords def) in (getf preset :midi-cc-fns)
             do (progn
                  (setf (apply #'aref *cc-fns* coords)
                        (eval def))
                  (funcall
                   (apply #'aref *cc-fns* coords)
                   (apply #'aref (getf preset :midi-cc-state) coords))))
          (loop for (chan def) in (getf preset :note-fns)
             do (progn
                  (setf (aref *note-fns* chan)
                        (eval def))
                  (funcall
                   (aref *note-fns* chan)
                   (aref (getf preset :midi-note-state) chan))))
          (setf *curr-preset* preset)
          (edit-preset-in-emacs *curr-preset*))))

#|
(progn
  (setf *curr-preset*
        (copy-list
         (append
          '(:boid-params
            (:num-boids nil
             :clockinterv 50
             :speed 2.0
             :obstacles-lookahead 2.5
             :maxspeed 0.85690904
             :maxforce 0.07344935
             :maxidx 317
             :length 5
             :sepmult 168/127
             :alignmult 343/127
             :cohmult 245/127
             :predmult 1
             :maxlife 60000.0
             :lifemult 100.0
             :max-events-per-tick 10)
            :audio-args
            (:pitchfn (+ 0.1 (* 0.6 y))
             :ampfn (* (sign) (+ (* 0.03 (expt 16 (- 1 y))) (random 0.01)))
             :durfn (* (expt 1/3 y) 1.8)
             :suswidthfn 0.1
             :suspanfn 0.1
             :decay-startfn 0.001
             :decay-endfn 0.002
             :lfo-freqfn (* 50 (expt 5 (/ (aref *cc-state* 0 7) 127))
                          (expt (+ 1 (* 1.1 (round (* 16 y)))) (expt 1.3 x)))
             :x-posfn x
             :y-posfn y
             :wetfn 1
             :filt-freqfn 20000)
            :midi-cc-fns
            (((0 0)
              (with-exp-midi (0.1 2)
                (let ((speedf (funcall ipfn d2)))
                  (set-value :maxspeed (* speedf 1.05))
                  (set-value :maxforce (* speedf 0.09)))))
             ((0 1)
              (with-lin-midi (1 8)
                (set-value :sepmult (funcall ipfn d2))))
             ((0 2)
              (with-lin-midi (1 8)
                (set-value :cohmult (funcall ipfn d2))))
             ((0 3)
              (with-lin-midi (1 8)
                (set-value :alignmult (funcall ipfn d2))))))
          `(:midi-cc-state ,(alexandria:copy-array *cc-state*)))))
  (digest-params *curr-preset*)
  (load-preset *curr-preset*))
|#

#|
(progn
  (digest-params *curr-preset*)
  (load-preset *curr-preset*))
|#

(defun store-curr-preset (num &key (presets *presets*))
  "store current preset as specified."
  (let ((state (copy-list *curr-preset*)))
    (setf (getf state :midi-cc-state) (alexandria:copy-array *cc-state*))
    (setf (aref presets num) state))
  (values))

;;; (store-curr-preset 1)


(defun state-store-curr-preset (num &key (presets *presets*))
  "store current preset but use the current state of the boid-params."
  (let ((state (copy-list *curr-preset*)))
    (setf (getf state :boid-params)
          (loop for (key val) on (getf *curr-preset* :boid-params) by #'cddr
             append (capture-param key val)))
    (setf (getf state :midi-cc-state) (alexandria:copy-array *cc-state*))
    (setf (aref presets num) state))
  (values))

;;; (state-store-curr-preset 1)

;;; (load-preset 5)
;;; (load-preset (aref *presets* 1))




(defun save-presets (&key (file *presets-file*))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(in-package :luftstrom-display)~%~%(setf *presets*~&~s)" *presets*))
  (format t "presets written to ~a" file)
  (format nil "presets written to ~a" file))

;;; (save-presets :file "presets/schwarm-18-10-03-02.lisp")

(defun load-presets (&key (file *presets-file*))
  (load file))

;;; (load-presets)
;; (load-preset (aref *presets* 0))

(defparameter *obst-move-time* 0.4)

#|
(setf (aref *note-fns* 0)
      (lambda (d1) (multiple-value-bind (x y) (keynum->coords d1)
                (time-move-obstacle-abs x y 0 *obst-move-time*))))
(setf (aref *note-fns* 1)
      (lambda (d1) (multiple-value-bind (x y) (keynum->coords d1)
                (time-move-obstacle-abs x y 0 *obst-move-time*))))
(setf (aref *note-fns* 2)
      (lambda (d1) (multiple-value-bind (x y) (keynum->coords d1)
                (time-move-obstacle-abs x y 0 *obst-move-time*))))
(setf (aref *note-fns* 3)
      (lambda (d1) (multiple-value-bind (x y) (keynum->coords d1)
                (time-move-obstacle-abs x y 0 *obst-move-time*))))
|#

(dotimes (player 4) (setf (aref *note-fns* player) #'identity))


;;; (setf *length* 105)

(defun move-test (time num)
  (if (> num 0)
      (let ((next (+ time 0.3)))
        (format t "step, ")
        (at next
          #'move-test next (decf num)))))

;;; (move-test (now) 4)

(defun time-move-obstacle-abs (x y player-ref &optional (time 0))
  "linearly move obstacle of player-ref to new x and y positions in
duration given by time (in seconds)."
  (let* ((window cl-boids-gpu::*win*)
         (bs (first (cl-boids-gpu::systems window)))
         old-x old-y)
    (if bs
        (progn
          (ocl:with-mapped-buffer
              (p1 (car (cl-boids-gpu::command-queues window))
                  (cl-boids-gpu::obstacles-pos bs) 4
                  :offset (* cl-boids-gpu::+float4-octets+ (obstacle-ref (aref *obstacles* player-ref)))
                  :write t)
            (setf old-x (cffi:mem-aref p1 :float 0))
            (setf old-y (- (glut:height window) (cffi:mem-aref p1 :float 1))))
          (let* ((maxpixels (max (abs (- x old-x))
                                 (abs (- y old-y))))
                 (frames (* time 60))
                 (num-steps (max 1 (min frames maxpixels)))
                 (dtime (/ time num-steps))
                 (now (now))
                 (dx (/ (- x old-x) num-steps))
                 (dy (/ (- y old-y) num-steps)))
;;            (format t "~&~a ~a ~a ~a" old-x old-y x y)
            (loop for count below num-steps
               for curr-x = (incf old-x dx) then (incf curr-x dx)
               for curr-y = (incf old-y dy) then (incf curr-y dy)
               do (at (+ now (float (* count dtime)))
                    #'cl-boids-gpu::move-obstacle-abs (round curr-x) (round curr-y) player-ref))))
        (setf (aref luftstrom-display::*note-state* player-ref)
              (luftstrom-display::coords->keynum x (- (glut:height window) y))))))

#|

(time-move-obstacle-abs 200 200 0 0.2)
(time-move-obstacle-abs 600 500 0 0.2)
(time-move-obstacle-abs 602 503 0 0.2)


(time-move-obstacle-abs 200 200 0)
(time-move-obstacle-abs 900 800 0)

|#


(defun make-move-fn (player &key (dir :up) (max 100) (ref nil) (clip nil))
  (let ((moving nil)
        (dv 0)
        (window cl-boids-gpu::*win*)
        (clip clip))
    (lambda (d2)
      (labels
          ((inner (time)
             (if moving
                 (let ((next (+ time 0.1)))
                   (move-obstacle-rel
                    player
                    (or (if ref (aref luftstrom-display::*note-state* player ref))
                        dir)
                    window :delta (m-exp dv 1 max)
                    :clip clip)
                   (at next #'inner next)))))
        (if (zerop d2) (setf moving nil)
            (progn
              (setf dv d2)
              (if (not moving)
                  (progn
                    (setf moving t)
                    (inner (now))))))))))

(defun make-move-fn2 (player &key (dir :up) (max 100) (ref nil) (clip nil))
  (let ((moving nil)
        (dv 0)
        (window cl-boids-gpu::*win*)
        (clip clip))
    (lambda (d2)
      (labels
          ((inner (time)
             (if moving
                 (let ((next (+ time 0.1)))
;;                   (format t "~&~a" (aref *cc-state** player ref))
                   (move-obstacle-rel
                    player
                    dir
                    window :delta (if ref (m-exp (aref *cc-state* player ref) 10 max)
                                      10)
                    :clip clip)
                   (at next #'inner next)))))
        (if (zerop d2) (setf moving nil)
            (progn
              (setf dv 1)
              (if (not moving)
                  (progn
                    (setf moving t)
                    (inner (now))))))))))



;;; (setf (mvobst-xtarget (aref *move-obst* 0)) 3.1)

;;; (setf (mvobst-xtarget (aref *move-obst* 0)) (+ x dv))
;;; (unless (mvobst-active (aref *move-obst* 0))



#|
(defparameter *my-mv-fn*
    (make-move-fn 0 :up 100 nil))

(funcall *my-mv-fn* 0)

bewegt sich immer so schnell, wie möglich zum Zielwert

bei Funktionsaufruf der Bewegungsfunktion:


|#

(defun make-move-fn3 (player &key (dir :up) (max 100) (ref nil) (clip nil))
  "assign a function which can be bound to be called each time, a new
event (like a cc value) is received."
  (let ((window cl-boids-gpu::*win*)
        (clip clip)
        (obstacle (obstacle player)))
    (lambda (d2)
      (labels ((inner (time)
                 "recursive function (with time delay between calls)
moving the obstacle framewise. As different gestures could trigger an
instance of the function (assigning a new instance by calling
'make-move-fn3) only one of it is run at a time for each
obstacle (assured by testing the 'active slot in the mvobst
struct). The target-dx and target-dy slots can get reassigned by all
assigned gestures while the function is running. The function
terminates if the dx and dy are both 0, (setting the 'active slot back
to nil so that it can get retriggered)."
                 (let ((obstacle obstacle))
                   (with-slots (target-dx target-dy) obstacle
                     (let* ((x-dist (abs target-dx))
                            (y-dist (abs target-dy))
                            (max-dist (max x-dist y-dist))
                            (dx 0) (dy 0))
                       (if (> max-dist 0)
                           (progn
                             (unless (zerop x-dist)
                               (if (< x-dist 10)
                                   (setf dx (signum target-dx))
                                   (setf dx (round (/ target-dx 10))))
                               (decf target-dx dx))
                             (unless (zerop y-dist)
                               (if (< y-dist 10)
                                   (setf dy (signum target-dy))
                                   (setf dy (round (/ target-dy 10))))
                               (decf target-dy dy))
                             (move-obstacle-rel-xy player dx dy window :clip clip)
                             (let ((next (+ time (/ 60.0)))) (at next #'inner next)))
                           (setf (obstacle-moving obstacle) nil)))))))
    ;;; lambda-function entry point
        (if (> d2 0)
            (let ((obstacle obstacle))
              (format t "~&received: ~a" d2)
              (case dir
                (:left (setf (obstacle-target-dx obstacle)
                             (float (* -1 (if ref (m-exp (aref *cc-state* player ref) 10 max) 10.0)))))
                (:right (setf (obstacle-target-dx obstacle)
                              (float (if ref (m-exp (aref *cc-state* player ref) 10 max) 10.0))))
                (:down (setf (obstacle-target-dy obstacle)
                             (float (* -1 (if ref (m-exp (aref *cc-state* player ref) 10 max) 10.0)))))
                (:up (setf (obstacle-target-dy obstacle)
                           (float (if ref (m-exp (aref *cc-state* player ref) 10 max) 10.0)))))
              (unless (obstacle-moving obstacle)
                (setf (obstacle-moving obstacle) t)
                (inner (now)))))))))

#|

(defun make-retrig-move-fn (player &key (dir :up) (num-steps 10) (max 100) (ref nil) (clip nil))
  "return a function moving the obstacle of a player in a direction
specified by :dir which can be bound to be called each time, a new
event (like a cc value) is received. If ref is specified it points to
a cc value stored in *cc-state* which is used for exponential interpolation
of the boid's stepsize between 10 and :max pixels."
  (let* ((clip clip)
         (obstacle (obstacle player))
         (obstacle-ref (obstacle-ref obstacle))
         (retrig? nil))
    (lambda (d2)
      (labels ((retrig (time)
                 "recursive function (with time delay between calls)
simulating a repetition of keystrokes after a key is depressed (once)
until it is released."
                 (if retrig?
                     (let ((next (+ time 0.1)))
                       (progn
                         ;;                         (format t "~&received: ~a" d2)
                         (case dir
                           (:left (set-obstacle-dx
                                   obstacle-ref
                                   (float (* -1 (if ref (ou:m-exp-zero (aref *cc-state* player ref) 10 max) 10.0)))
                                   num-steps clip))
                           (:right (set-obstacle-dx
                                    obstacle-ref
                                    (float (if ref (ou:m-exp-zero (aref *cc-state* player ref) 10 max) 10.0))
                                    num-steps clip))
                           (:down (set-obstacle-dy
                                   obstacle-ref
                                   (float (* -1 (if ref (ou:m-exp-zero (aref *cc-state* player ref) 10 max) 10.0)))
                                   num-steps clip))
                           (:up (set-obstacle-dy
                                 obstacle-ref
                                 (float (if ref (ou:m-exp-zero (aref *cc-state* player ref) 10 max) 10.0))
                                 num-steps clip))))
                       ;;                       (format t "~&retrig, act: ~a" (obstacle-moving obstacle))
                       (at next #'retrig next)))))
;;; lambda-function entry point
        ;;        (format t "~&me-received: ~a" d2)
        (if (obstacle-active obstacle)
            (if (> d2 0)
                (unless retrig?
                  (setf retrig? t)
                  (retrig (now)))
                (setf retrig? nil)))))))
|#

(defun make-retrig-move-fn (player &key (dir :up) (num-steps 10) (max 100) (ref nil) (clip nil))
  "return a function moving the obstacle of a player in a direction
specified by :dir which can be bound to be called each time, a new
event (like a cc value) is received. If ref is specified it points to
a cc value stored in *cc-state* which is used for exponential interpolation
of the boid's stepsize between 0 and :max pixels."
  (let* ((clip clip)
         (obstacle (obstacle player))
         (obstacle-ref (obstacle-ref obstacle))
         (retrig? nil))
    (lambda (d2)
      (labels ((retrig (time)
                 "recursive function (with time delay between calls)
simulating a repetition of keystrokes after a key is depressed (once)
until it is released."
                 (if retrig?
                     (let ((next (+ time 0.1)))
                       (progn
;;                         (format t "~&received: ~a" d2)
                         (case dir
                           (:left (set-obstacle-dx
                                   obstacle-ref
                                   (float (* -1 (if ref (ou:m-exp-zero (aref *cc-state* player ref) 1 max) 10.0)))
                                   num-steps clip))
                           (:right (set-obstacle-dx
                                    obstacle-ref
                                    (float (if ref (ou:m-exp-zero (aref *cc-state* player ref) 1 max) 10.0))
                                    num-steps clip))
                           (:down (set-obstacle-dy
                                   obstacle-ref
                                   (float (* -1 (if ref (ou:m-exp-zero (aref *cc-state* player ref) 1 max) 10.0)))
                                   num-steps clip))
                           (:up (set-obstacle-dy
                                 obstacle-ref
                                 (float (if ref (ou:m-exp-zero (aref *cc-state* player ref) 1 max) 10.0))
                                 num-steps clip))))
                       ;;                       (format t "~&retrig, act: ~a" (obstacle-moving obstacle))
                       (at next #'retrig next)))))
;;; lambda-function entry point
        ;;        (format t "~&me-received: ~a" d2)
        (cond
          ((numberp d2)
           (if (obstacle-active obstacle)
               (if (> d2 0)
                   (unless retrig?
                     (setf retrig? t)
                     (retrig (now)))
                   (setf retrig? nil))))
          ((eq d2 'stop)
           (setf retrig? nil))
          (:else (warn "arg ~a not handled by make-retrig-move-fn." d2)))))))

;;; (defparameter *mv-test* (make-retrig-move-fn 0 :dir :up))

;;; (funcall *mv-test* 'stop)

;;; (funcall *mv-test* 10)
;;; (move-obstacle-rel-xy player dx dy window :clip clip)
;;; (setf (obstacle-moving (obstacle 0)) nil)

;; (setf (obstacle-moving (obstacle 0)) nil)

(defun std-obst-move (player max ref)
  `(((,player ,ref)
     (with-exp-midi-fn (1.0 100.0)
       (set-value :predmult (float (funcall ipfn d2)))))
    ((,player 40) (make-retrig-move-fn ,player :dir :right :max ,max :ref ,ref :clip nil))
    ((,player 50) (make-retrig-move-fn ,player :dir :left :max ,max :ref ,ref :clip nil))
    ((,player 60) (make-retrig-move-fn ,player :dir :up :max ,max :ref ,ref :clip nil))
    ((,player 70) (make-retrig-move-fn ,player :dir :down :max ,max :ref ,ref :clip nil))))

(declaim (inline register-cc-fn))
(defun register-cc-fn (fn)
  (push fn *curr-cc-fns*))

(declaim (inline deactivate-cc-fns))
(defun deactivate-cc-fns ()
  (dolist (fn *curr-cc-fns*)
    (funcall fn 'stop))
  (setf *curr-cc-fns* nil))

(defparameter *default-audio-preset* (make-list 19))

(defparameter *audio-fn-id-lookup*
  (let ((hash (make-hash-table)))
    (loop for key in '(:preset-form :p1 :p2 :p3 :p4 :pitchfn :ampfn :durfn :suswidthfn :suspanfn :decay-startfn
                       :decay-endfn :lfo-freqfn :x-posfn :y-posfn :wetfn :filt-freqfn :bp-freq :bp-rq)
       for id from 0
       do (setf (gethash key hash) id))
    hash))

(defun new-audio-preset ()
  (make-array 19 :initial-contents *default-audio-preset*))

(defun get-fn-idx (key)
  (gethash key *audio-fn-id-lookup*))

(defun digest-audio-args-preset (args &optional audio-preset)
  (let ((preset (or audio-preset (new-audio-preset))))
    (loop
       for (key val) on args by #'cddr
       for idx = (get-fn-idx key)
       do (setf (aref preset idx)
                (eval `(lambda (&optional x y v tidx p1 p2 p3 p4)
                         (declare (ignorable x y v tidx p1 p2 p3 p4))
                          ,val))))
    (setf (aref preset 0) args)
    preset))

(setf *default-audio-preset*
  (coerce
   (digest-audio-args-preset
    '(:p1 1
      :p2 (- p1 1)
      :p3 0
      :p4 0
      :pitchfn (+ p2 (n-exp y 0.4 1.08))
      :ampfn (progn (* (/ v 20) (sign) (n-exp y 3 1.5)))
      :durfn 0.5
      :suswidthfn 0
      :suspanfn (random 1.0)
      :decay-startfn 5.0e-4
      :decay-endfn 0.002
      :lfo-freqfn (r-exp 50 80)
      :x-posfn x
      :y-posfn y
      :wetfn 1
      :filt-freqfn 20000
      :bp-freq 500
      :bp-rq 10))
   'list))

(defparameter *audio-presets*
  (let ((size 128))
    (make-array size :initial-contents (loop for idx below size collect (new-audio-preset)))))

(defparameter *curr-audio-presets*
  (let ((size 20))
    (make-array size :initial-contents (loop for idx below size collect (new-audio-preset)))))

(defmacro apr (ref)
  `(elt ,*audio-presets* ,ref))

#|
(defun preset-fn (key preset)
  (aref preset (get-fn-idx key)))
|#

(defun audio-preset-form (audio-preset)
  (elt audio-preset 0))

(defun get-audio-preset-string (audio-preset)
  (with-output-to-string (out)
    (loop for (key value) on (audio-preset-form audio-preset) by #'cddr
          for start = "'(" then #\NEWLINE
          do (format out "~a~s ~a" start key value))
    (format out ")")))

;;; (get-audio-preset-string (elt *audio-presets* 0))

(defun get-audio-preset-load-form (preset-no)
  (with-output-to-string (out)
    (format out "(digest-audio-args-preset~%")
    (format out (get-audio-preset-string
                 (aref *audio-presets* preset-no)))
    
    (format out "~&(aref *audio-presets* ~a))~%" preset-no)))

(defun cp-audio-preset (src target)
  (setf (aref *audio-presets* target)
        (digest-audio-args-preset
         (elt (aref *audio-presets* src) 0))))

(defun save-audio-presets (&key (file *audio-presets-file*))
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (format out "(in-package :luftstrom-display)~%~%(progn~%")
    (loop for preset across *audio-presets*
          for idx from 0
          do (format out (get-audio-preset-load-form idx)))
    (format out ")~%"))
  (format t "audio-presets written to ~a" file)
  (format nil "audio-presets written to ~a" file))

;;; (save-audio-presets)

(defun load-audio-presets (&key (file *audio-presets-file*))
  (load file))

;;; (load-audio-presets)

;;; (read-from-string (get-audio-preset-load-form 0))

(defun save-all-presets ()
  (save-presets)
  (save-audio-presets))

;;; (save-all-presets)
;;; (edit-audio-preset-in-emacs 2)

(defparameter *cc-presets* (make-hash-table))

(defun init-cc-presets ()
  (loop for (key val) in
        `((:nk2-std ,(lambda (player)
                       `((,player 0)
                         (with-exp-midi-fn (0.1 20)
                           (let ((speedf (float (funcall ipfn d2))))
                             (set-value :maxspeed (* speedf 1.05))
                             (set-value :maxforce (* speedf 0.09))))
                         (,player 1)
                         (with-lin-midi-fn (1 8)
                           (set-value :sepmult (float (funcall ipfn d2))))
                         (,player 2)
                         (with-lin-midi-fn (1 8)
                           (set-value :cohmult (float (funcall ipfn d2))))
                         (,player 3)
                         (with-lin-midi-fn (1 8)
                           (set-value :alignmult (float (funcall ipfn d2))))
                         (,player 4)
                         (with-lin-midi-fn (0 500)
                           (set-value :lifemult (float (funcall ipfn d2)))))))
          (:nk2-std-noreset ,(lambda (player)
                               `((,player 0)
                                 (values (with-exp-midi-fn (0.1 20)
                                           (let ((speedf (float (funcall ipfn d2))))
                                             (set-value :maxspeed (* speedf 1.05))
                                             (set-value :maxforce (* speedf 0.09))))
                                         t)
                                 (,player 1)
                                 (values
                                  (with-lin-midi-fn (1 8)
                                    (set-value :sepmult (float (funcall ipfn d2))))
                                  t)
                                 (,player 2)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :cohmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 3)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :alignmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 4)
                                 (values (with-lin-midi-fn (0 500)
                                           (set-value :lifemult (float (funcall ipfn d2))))
                                         t))))
          (:nk2-std2 ,(lambda (player)
                        `((,player 0)
                          (with-exp-midi-fn (0.1 20)
                            (let ((speedf (float (funcall ipfn d2))))
                              (set-value :maxspeed (* speedf 1.05))
                              (set-value :maxforce (* speedf 0.09))))
                          (,player 1)
                          (with-lin-midi-fn (1 8)
                            (set-value :sepmult (float (funcall ipfn d2))))
                          (,player 2)
                          (with-lin-midi-fn (1 8)
                            (set-value :cohmult (float (funcall ipfn d2))))
                          (,player 3)
                          (with-lin-midi-fn (1 8)
                            (set-value :alignmult (float (funcall ipfn d2))))
                          (,player 4)
                          (with-lin-midi-fn (0 500)
                            (set-value :lifemult (float (funcall ipfn d2)))))))
          (:nk2-std2-noreset ,(lambda (player)
                               `((,player 0)
                                 (values (with-exp-midi-fn (0.1 20)
                                           (let ((speedf (float (funcall ipfn d2))))
                                             (set-value :maxspeed (* speedf 1.05))
                                             (set-value :maxforce (* speedf 0.09))))
                                         t)
                                 (,player 1)
                                 (values
                                  (with-lin-midi-fn (1 8)
                                    (set-value :sepmult (float (funcall ipfn d2))))
                                  t)
                                 (,player 2)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :cohmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 3)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :alignmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 4)
                                 (values (with-lin-midi-fn (0 500)
                                           (set-value :lifemult (float (funcall ipfn d2))))
                                         t))))

          (:nk2-mass ,(lambda (player)
                       `((,player 0)
                         (with-exp-midi-fn (0.1 20)
                           (let ((speedf (float (funcall ipfn d2))))
                             (set-value :maxspeed (* speedf 1.05))
                             (set-value :maxforce (* speedf 0.09))))
                         (,player 1)
                         (with-lin-midi-fn (1 8)
                           (set-value :sepmult (float (funcall ipfn d2))))
                         (,player 2)
                         (with-lin-midi-fn (1 8)
                           (set-value :cohmult (float (funcall ipfn d2))))
                         (,player 3)
                         (with-lin-midi-fn (1 8)
                           (set-value :alignmult (float (funcall ipfn d2))))
                         (,player 4)
                         (with-lin-midi-fn (0 100)
                           (set-value :lifemult (float (funcall ipfn d2)))))))
          (:nk2-mass-noreset ,(lambda (player)
                               `((,player 0)
                                 (values (with-exp-midi-fn (0.1 20)
                                           (let ((speedf (float (funcall ipfn d2))))
                                             (set-value :maxspeed (* speedf 1.05))
                                             (set-value :maxforce (* speedf 0.09))))
                                         t)
                                 (,player 1)
                                 (values
                                  (with-lin-midi-fn (1 8)
                                    (set-value :sepmult (float (funcall ipfn d2))))
                                  t)
                                 (,player 2)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :cohmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 3)
                                 (values (with-lin-midi-fn (1 8)
                                           (set-value :alignmult (float (funcall ipfn d2))))
                                         t)
                                 (,player 4)
                                 (values (with-lin-midi-fn (0 100)
                                           (set-value :lifemult (float (funcall ipfn d2))))
                                         t))))
          (:obst-ctl1 ,(lambda (player)
                         `((,player 7)
                           (lambda (d2)
                             (if (numberp d2)
                                 (let ((obstacle (aref *obstacles* ,player)))
                                   (with-slots (brightness radius)
                                       obstacle
                                     (let ((ipfn (ip-exp 2.5 10.0 128)))
                                       (set-lookahead ,player (float (funcall ipfn d2))))
                                     (let ((ipfn (ip-exp 1 1.0 128)))
                                       (set-multiplier ,player (float (funcall ipfn d2))))
                                     (let ((ipfn (ip-lin 0.2 1.0 128)))
                                       (setf brightness (funcall ipfn d2)))))))
                           (,player 40)
                           (make-retrig-move-fn ,player :dir :right :max 400 :ref 7 :clip nil)
                           (,player 50)
                           (make-retrig-move-fn ,player :dir :left :max 400 :ref 7 :clip nil)
                           (,player 60)
                           (make-retrig-move-fn ,player :dir :up :max 400 :ref 7 :clip nil)
                           (,player 70)
                           (make-retrig-move-fn ,player :dir :down :max 400 :ref 7 :clip nil)
                           (,player 99)
                           (lambda (d2)
                             (if (and (numberp d2) (= d2 127))
                                 (toggle-obstacle ,player)))
                           )))
          (:obst-ctl2 ,(lambda (player)
                         `((,player 7)
                           (lambda (d2)
                             (if (numberp d2)
                                 (let ((obstacle (aref *obstacles* ,player)))
                                   (with-slots (brightness radius)
                                       obstacle
                                     (let ((ipfn (ip-exp 2.5 2.5 128)))
                                       (set-lookahead ,player (float (funcall ipfn d2))))
                                     (let ((ipfn (ip-exp 1 100.0 128)))
                                       (set-multiplier ,player (float (funcall ipfn d2))))
                                     (let ((ipfn (ip-lin 0.2 1.0 128)))
                                       (setf brightness (funcall ipfn d2)))))))
                           (,player 40)
                           (make-retrig-move-fn ,player :dir :right :max 400 :ref 7 :clip nil)
                           (,player 50)
                           (make-retrig-move-fn ,player :dir :left :max 400 :ref 7 :clip nil)
                           (,player 60)
                           (make-retrig-move-fn ,player :dir :up :max 400 :ref 7 :clip nil)
                           (,player 70)
                           (make-retrig-move-fn ,player :dir :down :max 400 :ref 7 :clip nil)
                           (,player 99)
                           (lambda (d2)
                             (if (and (numberp d2) (= d2 127))
                                 (toggle-obstacle ,player)))
                           )))
          (:boid-ctl1-noreset ,(lambda (player)
                                 `((,player 100)
                                   (values
                                    (with-exp-midi-fn (0.1 20)
                                      (unless (= (aref *cc-state* ,player 40) 127)
                                        (let ((speedf (float (funcall ipfn d2))))
                                          (set-value :maxspeed (* speedf 1.05))
                                          (set-value :maxforce (* speedf 0.09)))))
                                    t)
                                   (,player 70)
                                   (values (with-lin-midi-fn (1 8)
                                             (unless (= (aref *cc-state* ,player 40) 127)
                                               (set-value :sepmult (float (funcall ipfn d2)))))
                                           t)
                                   (,player 65)
                                   (values (with-lin-midi-fn (1 8)
                                             (unless (= (aref *cc-state* ,player 40) 127)
                                               (set-value :cohmult (float (funcall ipfn d2)))))
                                           t)
                                   (,player 7)
                                   (values (with-lin-midi-fn (1 8)
                                             (unless (= (aref *cc-state* ,player 40) 127)
                                               (set-value :alignmult (float (funcall ipfn d2)))))
                                           t)
                                   (,player 40)
                                   (lambda (d2)
                                     (if (and (numberp d2) (> d2 0))
                                         (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                                   (,player 50)
                                   (lambda (d2)
                                     (if (and (numberp d2) (> d2 0))
                                         (cl-boids-gpu::timer-add-boids *boids-per-click* 50))))))
          (:boid-ctl1 ,(lambda (player)
                                 `((,player 100)
                                   (with-exp-midi-fn (0.1 20)
                                     (unless (= (aref *cc-state* ,player 40) 127)
                                       (let ((speedf (float (funcall ipfn d2))))
                                         (set-value :maxspeed (* speedf 1.05))
                                         (set-value :maxforce (* speedf 0.09)))))
                                   (,player 70)
                                   (with-lin-midi-fn (1 8)
                                     (unless (= (aref *cc-state* ,player 40) 127)
                                       (set-value :sepmult (float (funcall ipfn d2)))))
                                   (,player 65)
                                   (with-lin-midi-fn (1 8)
                                     (unless (= (aref *cc-state* ,player 40) 127)
                                       (set-value :cohmult (float (funcall ipfn d2)))))
                                   (,player 7)
                                   (with-lin-midi-fn (1 8)
                                     (unless (= (aref *cc-state* ,player 40) 127)
                                       (set-value :alignmult (float (funcall ipfn d2)))))
                                   (,player 40)
                                   (lambda (d2)
                                     (if (and (numberp d2) (> d2 0))
                                         (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                                   (,player 50)
                                   (lambda (d2)
                                     (if (and (numberp d2) (> d2 0))
                                         (cl-boids-gpu::timer-add-boids *boids-per-click* 50))))))
          (:boid-ctl2 ,(lambda (player)
                         `((,player 7)
                           (with-exp-midi-fn (0.1 20)
                             (unless (= (aref *cc-state* ,player 40) 127)
                               (let ((speedf (float (funcall ipfn d2))))
                                 (set-value :maxspeed (* speedf 1.05))
                                 (set-value :maxforce (* speedf 0.09)))))
                           (,player 70)
                           (with-lin-midi-fn (1 8)
                             (unless (= (aref *cc-state* ,player 40) 127)
                               (set-value :sepmult (float (funcall ipfn d2)))))
                           (,player 65)
                           (with-lin-midi-fn (1 8)
                             (unless (= (aref *cc-state* ,player 40) 127)
                               (set-value :cohmult (float (funcall ipfn d2)))))
                           (,player 100)
                           (with-lin-midi-fn (1 8)
                             (unless (= (aref *cc-state* ,player 40) 127)
                               (set-value :alignmult (float (funcall ipfn d2)))))
                           (,player 40)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                           (,player 50)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-add-boids *boids-per-click* 50
                                                                (list
                                                                 (list (obstacle-x (aref *obstacles* ,player))
                                                                       (obstacle-y (aref *obstacles* ,player))))))))))
          (:life-ctl1 ,(lambda (player)
                         `((,player 7)
                           (with-lin-midi-fn (0 100)
                             (set-value :lifemult (float (funcall ipfn d2))))
                           (,player 40)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                           (,player 50)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-add-boids *boids-per-click* 50)))
                           )))
          (:life-ctl3 ,(lambda (player)
                         `((,player 7)
                           (with-lin-midi-fn (0 600)
                             (set-value :lifemult (float (funcall ipfn d2))))
                           (,player 40)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                           (,player 50)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-add-boids *boids-per-click* 50)))
)))
          (:life-ctl2 ,(lambda (player)
                         `((,player 100)
                           (lambda (d2)
                             (if (numberp d2)
                                 (let ((obstacle (aref *obstacles* ,player)))
                                   (with-slots (brightness) obstacle
                                     (let ((ipfn (ip-lin 0.2 1.0 128)))
                                       (setf brightness (funcall ipfn d2)))))))
                           (,player 70)
                           (with-lin-midi-fn (0 100)
                             (set-value :lifemult (float (funcall ipfn d2))))
                           (,player 40)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-remove-boids *boids-per-click* 50)))
                           (,player 50)
                           (lambda (d2)
                             (if (and (numberp d2) (> d2 0))
                                 (cl-boids-gpu::timer-add-boids *boids-per-click* 50
                                                                (list
                                                                 (list (obstacle-x (aref *obstacles* ,player))
                                                                       (obstacle-y (aref *obstacles* ,player)))))))
                           (,player 99)
                           (lambda (d2)
                             (if (and (numberp d2) (= d2 127))
                                 (toggle-obstacle ,player))))))
          

          )
        do (setf (gethash key *cc-presets*) val)))

(init-cc-presets)

#|
(eval (second (funcall (gethash :boid-ctl1 *cc-presets*) 3)))
|#

(defun cc-preset (player key)
  (if (keywordp player)
      (funcall (gethash key *cc-presets*) (gethash player *player-lookup*))
      (funcall (gethash key *cc-presets*) player)))


;; (cc-preset :player2 :obst-ctl1)
;; (cc-preset 0 :obst-ctl1)

;;; (load-preset 0)

(defun expand-audio-def (def)
  (cond
    ((null def) ())
    ((numberp def) def)
    ((symbolp def) def)
    ((consp (first def))
     (cons (expand-audio-def (first def)) (expand-audio-def (rest def))))
    (t (if (and
            (eql (first def) 'aref)
            (eql (second def) '*cc-state*))
           (eval def)
           (cons (first def) (expand-audio-def (rest def)))))))

(defun extract-preset (ref)
  (loop for (key val) on (elt (elt *audio-presets* ref) 0) by #'cddr
        append (list key (expand-audio-def val))))

(defun ewi-lin (x min max)
  (+ min (* (- max min) (/ (- x 24) 84))))

(defun ewi-nlin (tidx min max)
  (+ min (* (- max min) (/ (- (player-note tidx) 24) 84))))
