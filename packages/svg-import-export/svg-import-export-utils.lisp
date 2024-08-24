;;; svg-import-export-utils.lisp
;;;
;;; assorted utility functions for creating music/midi-piano-roll
;;; style backgrounds, exporting and importing and some personal
;;; pd-related stuff.
;;;
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package #:svg-import-export)

(defparameter *color-lookup* nil)

(defun get-p-dur (start l)
  (loop
     for x in l
     if (< start (first x)) return (return (- (first x) start))
     finally (return 1)))

(defun adjust-timing (list &key (timescale 1) (dynamik 20))
  (let* ((offs 0)
         (min-pitch (apply #'min (mapcar #'second list)))
         (max-pitch (apply #'max (mapcar #'second list)))
         (get-vol (lambda (pitch) (+ 96
                                 (* (- min-pitch pitch)
                                    (/ dynamik (- max-pitch min-pitch))))))
         (last-elem (copy-list (first (nthcdr (- (length list) 1) list)))))
    (loop
       for point in list
       for plist on (append (cdr list) (list (cons (+ (first last-elem) 1) (cdr last-elem))))
       collect (destructuring-bind (x y) point
                 (list (* (+ x offs) timescale) y (funcall get-vol y) (* timescale (get-p-dur x plist)))))))


#|
(append
   (loop for startpitch from 12 to 108 by 1 ;; 1 is interval from white black key in one octave to first white key in next.
      append (mapcar (lambda (x) (incf startpitch x)) ;;; note that startpitch gets destructively modified here!
                     '(0 2 2 1 2 2 2))) ;;; intervals of white keys in one octave
   '(120))
|#

;;; keynums of white keys on 88-key keyboard.

(defparameter *white-keys*
  '(12 14 16 17 19 21 23
    24 26 28 29 31 33 35
    36 38 40 41 43 45 47
    48 50 52 53 55 57 59
    60 62 64 65 67 69 71
    72 74 76 77 79 81 83
    84 86 88 89 91 93 95
    96 98 100 101 103 105 107
    108 110 112 113 115 117 119
    120))

#|

(loop for startpitch from 13 to 109 by 3   ;;; 3 is interval from last black key in one octave to first black key in next.
   append (mapcar (lambda (x) (incf startpitch x)) ;;; note that startpitch gets destructively modified here!
                  '(0 2 3 2 2)))           ;;; intervals of black keys in one octave

|#

;;; keynums of black keys on 88-key keyboard.

(defparameter *black-keys*
  '(13 15 18 20 22
    25 27 30 32 34
    37 39 42 44 46
    49 51 54 56 58
    61 63 66 68 70
    73 75 78 80 82
    85 87 90 92 94
    97 99 102 104 106
    109 111 114 116 118))

;;; keynums of position of stafflines for g-15ma, G, F and F_16bassa
;;; clefs.

(defparameter *staff-lines*
  '(19 23 26 29 33
    43 47 50 53 57
    64 67 71 74 77
    88 91 95 98 101
    112 115 119))

;;; keynums of position of ledgerlines for g-15ma, G, F and F_16bassa
;;; clefs.

(defparameter *ledger-lines*
  '(12 16
    36 40
    60
    81 84
    105 108
    ))

;;; create a midi-editor style piano-roll layer with horizontal white
;;; and grey stripes.

(defun svg-piano-roll (svg-file &key (visible t) (width 10000))
  (append (list (make-instance 'svg-tl-layer :name "PianoRoll" :id (new-id svg-file 'layer-ids)
                               :insensitive t
                               :visible visible))
          (list (cons (make-instance 'svg-group
                                     :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *white-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (- pitch 0.5)
                                                 :width width
                                                 :height 1
                                                 :stroke-color "#aaaaaa"
                                                 :stroke-width 0.1
                                                 :id (new-id svg-file 'rect-ids)))
                       (loop for pitch in *black-keys*
                          collect (make-instance 'svg-rect 
                                                 :x 0
                                                 :y (- pitch 0.5)
                                                 :width width
                                                 :height 1
                                                 :stroke-width 0.1
                                                 :stroke-color "#aaaaaa"
                                                 :fill-color "#dddddd"
                                                 :id (new-id svg-file 'rect-ids))))))))


(defun add-piano-roll (svg-file &rest args)
  (let ((width (or (getf args :width) (width svg-file))))
    (push
     (append (list (make-instance 'svg-tl-layer :name "PianoRoll"
                                                :id (new-id svg-file 'layer-ids)
                                                :insensitive t
                                                :visible (getf args :visible t)))
             (list (cons (make-instance 'svg-group
                                        :id (new-id svg-file 'group-ids))
                         (append
                          (loop for pitch in *white-keys*
                                collect (make-instance 'svg-rect 
                                                       :x 0
                                                       :y (- pitch 0.5)
                                                       :width width
                                                       :height 1
                                                       :stroke-color "#aaaaaa"
                                                       :stroke-width 0.1
                                                       :id (new-id svg-file 'rect-ids)))
                          (loop for pitch in *black-keys*
                                collect (make-instance 'svg-rect 
                                                       :x 0
                                                       :y (- pitch 0.5)
                                                       :width width
                                                       :height 1
                                                       :stroke-width 0.1
                                                       :stroke-color "#aaaaaa"
                                                       :fill-color "#dddddd"
                                                       :id (new-id svg-file 'rect-ids)))))))
     (elements svg-file))))

;;; create a staffsystem layer with ledger lines for G^15ma, G, F and
;;; F_16bassa clefs. The clefs are not drawn and the distance of
;;; neighboring lines is poportional to the interval between them
;;; (major/minor third). This means that a juxtaposition of piano-roll
;;; and staff-systems will align them correctly.

(defun svg-staff-system (svg-file &key (visible t) (width 10000))
  (append (list (make-instance 'svg-tl-layer :name "Stafflines" :id (new-id svg-file 'layer-ids) :insensitive t
                               :visible visible))
          (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                      (append
                       (loop for pitch in *staff-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 pitch
                                                 :x2 width
                                                 :y2 pitch
                                                 :stroke-color "#000000"
                                                 :stroke-width 0.2
                                                 :id (new-id svg-file 'line-ids)))
                       (loop for pitch in *ledger-lines*
                          collect (make-instance 'svg-line
                                                 :x1 0
                                                 :y1 pitch
                                                 :x2 width
                                                 :y2 pitch
                                                 :stroke-color "#eeeeee"
                                                 :stroke-width 0.4
                                                 :id (new-id svg-file 'line-ids))))))))

(defun add-staff-system (svg-file &rest args)
  (let ((width (or (getf args :width) (width svg-file))))
    (push
     (append (list (make-instance 'svg-tl-layer :name "Stafflines" :id (new-id svg-file 'layer-ids) :insensitive t
                                                :visible (getf args :visible t)))
             (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                         (append
                          (loop for pitch in *staff-lines*
                                collect (make-instance 'svg-line
                                                       :x1 0
                                                       :y1 pitch
                                                       :x2 width
                                                       :y2 pitch
                                                       :stroke-color "#000000"
                                                       :stroke-width 0.2
                                                       :id (new-id svg-file 'line-ids)))
                          (loop for pitch in *ledger-lines*
                                collect (make-instance 'svg-line
                                                       :x1 0
                                                       :y1 pitch
                                                       :x2 width
                                                       :y2 pitch
                                                       :stroke-color "#eeeeee"
                                                       :stroke-width 0.4
                                                       :id (new-id svg-file 'line-ids)))))))
     (elements svg-file))))

(defun svg-barlines (svg-file &key (visible t) (width 10000) (x-scale 8) (barstepsize 4) (startbar 1) (barmultiplier 1) timesigs)
  (let ((times (if timesigs
                   (loop
                     for time = 0 then (incf time
                                             (* x-scale barwidth))
                     for barwidth in timesigs
                     while (< time width)
                     collect time)
                   (loop
                     for time = 0
                       then (incf time (* x-scale barstepsize))
                     while (< time width)
                     collect time))))
    (append (list (make-instance 'svg-layer :name "Barlines" :id (new-id svg-file 'layer-ids) :insensitive t
                                            :visible visible))
            (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                        (append
                         (loop
                           for idx from 0
                           for time in times
                           collect (make-instance 'svg-line
                                                  :x1 (float time)
                                                  :y1 29.5
                                                  :x2 (float time)
                                                  :y2 138.50
                                                  :stroke-color "#000000"
                                                  :stroke-width 0.25
                                                  :id (new-id svg-file 'line-ids))))))
            (list (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                        (append
                         (loop for time in times
                               for barnum from startbar by barmultiplier
                               collect (make-instance 'svg-text
                                                      :label (format nil "~d" barnum)
                                                      :x (float time)
                                                      :y 29
                                                      :fill-color +black+
                                                      :font-size 6
                                                      :id (new-id svg-file 'text-ids)))))))))


;;; transform simple lists containing x,y and optional color or opacity
;;; to svg circle objects. Sublists are treated as groups.

(defun points->svg (list svg-file &key color opacity radius)  
  (loop for point in list
     collect
       (if (numberp (car point))
           (destructuring-bind (cx cy &optional (l-color "#000000") (l-opacity 1.0) (l-radius 0.5)) point
             (make-instance 'svg-point
                            :cx cx :cy cy 
                            :rx (or radius l-radius) :ry (or radius l-radius)
                            :stroke-width 0
                            ;;                                      :stroke-color (color-lookup color) 
                            :opacity (or opacity l-opacity)
                            :fill-color (or color l-color +black+)
                            :id (new-id svg-file 'point-ids)))
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (points->svg point svg-file :color color :opacity opacity)))))

;;; transform simple lists containing x, y, width and optional color or
;;; opacity to horizontal svg line objects. Sublists are treated as groups.

(defun lines->svg (list svg-file &key color opacity (stroke-width 0.5))  
  (loop for line in list
     collect
       (if (numberp (car line))
           (destructuring-bind (x1 y1 width &optional l-color l-opacity) line
             (make-instance 'svg-line :x1 x1 :y1 y1
                            :x2 (+ x1 width) :y2 y1
                            :stroke-width stroke-width
                            :opacity (or opacity l-opacity 1)
                            :stroke-color (or color l-color +black+) 
                            ;; :fill-color (or l-color color)
                            :id (new-id svg-file 'line-ids)))
           (cons (make-instance 'svg-group :id (new-id svg-file 'group-ids))
                 (lines->svg line svg-file :color color :opacity opacity)))))

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

(defun svg->points (&key (infile #P"/tmp/test.svg") (timescale 1) (xquantize t) (yquantize t) (x-offset 0) (layer "Punkte"))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile.
Also removes duplicates and flattens subgroups. Points are simple
two-element lists containing x and y coordinates. The y coordinate is
supposed to be a midifloat value, x ist translated into secs/beats."
  (sort
   (remove-duplicates
    (flatten-fn (get-points-from-file :fname infile :timescale timescale :x-offset x-offset :xquantize xquantize :yquantize yquantize :layer-name layer) :fn #'caar)
    :test #'equal)
   (lambda (x y) (< (first x) (first y)))))

#|
(defun svg->lines (&key (infile #P"/tmp/test.svg") (timescale 1) (xquantize t) (yquantize t) (x-offset 0) (layer "Punkte"))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile.
Also removes duplicates and flattens subgroups. Points are simple
two-element lists containing x and y coordinates. The y coordinate is
supposed to be a midifloat value, x ist translated into secs/beats."
  (sort
   (remove-duplicates
    (flatten-fn (get-lines-from-file :fname infile :timescale timescale :x-offset x-offset :xquantize xquantize :yquantize yquantize :layer-name layer) :fn #'caar)
    :test #'equal)
   (lambda (x y) (< (first x) (first y)))))
|#

(defun svg->lines (&key (infile #P"/tmp/test.svg") (timescale 1) (xquantize nil) (yquantize nil) (layer "Events") group? layer?)
  "extract all line objects in the layer \"Events\" of svg infile.
Also removes duplicates and flattens subgroups. Lines are property
lists containing all svg attributes."
  (let ((seq (svg-get-lines-from-file :fname infile :timescale timescale :xquantize xquantize :yquantize yquantize
                                      :layer-name layer :layer? layer?)))
    (if group? seq
        (ou:flatten seq))))

(defun xscale-points (points xscale &key (xoffs 0))
  (cond ((null points) '())
        ((consp (caar points))
         (cons (xscale-points (car points) xscale :xoffs xoffs)
               (xscale-points (cdr points) xscale :xoffs xoffs)))
        (t (mapcar (lambda (p) (cons (* xscale (+ xoffs (first p))) (rest p)))
                   points))))

(defun xscale-lines (lines xscale  &key (xoffs 0))
  (cond ((null lines) '())
        ((consp (caar lines))
         (cons (xscale-lines (car lines) xscale :xoffs xoffs)
               (xscale-lines (cdr lines) xscale :xoffs xoffs)))
        (t (mapcar (lambda (p) (append (list (* xscale (+ xoffs (first p))) (second p)
                                        (* xscale (+ xoffs (third p))))
                                  (nthcdr 3 p)))
                   lines))))

;;; (xscale-lines '(((0 3 2) (2 4 1)) (2 -1 4) (3 5 3)) 0.5)


;; regenerate-points reads points from an svg file, quantizes them and
;; creates a new list of points. This is useful, if groups of points
;; have been transformed with skew or scaling operations and the
;; points are distorted to ellipsoids or scaled.

(defun regenerate-points (svg-file &key (fname #P"/tmp/test.svg") (xquantize t) (yquantize t))
  (append 
   (list (make-instance 'svg-tl-layer :name "Punkte" :id (new-id svg-file 'layer-ids)))
   (points->svg (get-points-from-file :fname fname :xquantize xquantize :yquantize yquantize) svg-file)))

;;; as the name specifies, an input file is read and a new file is
;;; exported with all points set to their original shape.

(defun renew-svg-file (&key (infile #P"/tmp/test.svg")
                         (outfile #P"/tmp/test.svg"))
  (let ((svg-file (make-instance 'svg-file)))
    (setf (slot-value svg-file 'elements) 
          (list
           (svg-piano-roll svg-file)
           (svg-staff-system svg-file)
           (regenerate-points svg-file :fname infile)
           ))
    (export-svg-file svg-file :fname outfile)))

;; (renew-svg-file)

(defun get-tick-lines (end-time svg-file &key
                                           (color "#000000")
                                           (start 0) (interval 10) (length 5) (non-drawing-modulo nil)
                       (ypos 0) (gxoffs 20) (gyoffs 300) (xscale 1))
  (let ((modulo (if (numberp non-drawing-modulo)
                    (* non-drawing-modulo interval))))
    (loop
       for time from start to end-time by interval
       append (if (or (not modulo) (> (mod time modulo) 0))
                  (list (make-instance 'svg-line
                                       :id (new-id svg-file 'line-ids)
                                       :x1 (+ gxoffs (* xscale (- time start)))
                                       :x2 (+ gxoffs (* xscale (- time start)))
                                       :y1 (+ gyoffs ypos)
                                       :y2 (+ gyoffs ypos length)
                                       :stroke-width 0.2
                                       :fill-color color
                                       :stroke-color color))))))

;;; create a time axis with tick lines and text labels. The list of
;;; the ticks keyword defines, which tick lines are drawn. Each list
;;; element enables a tick line every 60, 30, 10, 5 and 1 seconds,
;;; respectively. Text labels are drawn every 30 seconds.
#|

(defun svg-zeitachse 
    (end-time svg-file &key 
                         (ypos -50)
                         (gxoffs 20)
                         (gyoffs 300)
                         (xscale 1)
                         (inverse nil)
                         (ticks '(t t t t t)))
  (let ((stroke-color (if inverse "#ffffff" "#000000")))
    (list
     (make-instance 'svg-layer 
                    :name "Zeitachse" 
                    :id (new-id svg-file 'layer-ids))
     (list
      (make-instance 'svg-group
                     :id (new-id svg-file 'group-ids))
      (append
       (list
        (make-instance 'svg-group
                       :id (new-id svg-file 'group-ids))
        (make-instance 'svg-line
                       :id (new-id svg-file 'line-ids)
                       :x1 gxoffs
                       :x2 (+ gxoffs (* xscale end-time))
                       :y1 (+ gyoffs ypos)
                       :y2 (+ gyoffs ypos)
                       :stroke-width 0.2
                       :fill-color stroke-color
                       :stroke-color stroke-color))
       (if (nth 0 ticks) 
           (get-tick-lines end-time svg-file 
                           :color stroke-color
                           :interval 60
                           :length 10
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 1 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 30
                           :non-drawing-modulo 2
                           :length 8
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 2 ticks)
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 10
                           :non-drawing-modulo 3
                           :length 5
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 3 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 5
                           :non-drawing-modulo 2
                           :length 3
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 4 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 1
                           :non-drawing-modulo 5
                           :length 1
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos)))
      (cons (make-instance 'svg-group
                           :id (new-id svg-file 'group-ids))
            (get-time-text end-time svg-file
                           :color stroke-color
                           :xscale xscale
                           :gxoffs (- gxoffs 7.5)
                           :gyoffs gyoffs
                           :ypos (- ypos 2)))))))
|#

(defun svg-zeitachse 
    (end-time svg-file &key
                         (inverse nil)
                         (ypos -50)
                         (gxoffs 20)
                         (gyoffs 300)
                         (xscale 1)
                         (ticks '(t t t t t)))
  
  (let ((stroke-color (if inverse "#ffffff" "#000000")))  
    (add-elements
     (make-instance 'svg-layer 
                    :name "Zeitachse" 
                    :id (new-id svg-file 'layer-ids))
     (add-elements
      (make-instance 'svg-group
                     :id (new-id svg-file 'group-ids))
      (add-elements
       (make-instance 'svg-group
                      :id (new-id svg-file 'group-ids))
       (make-instance 'svg-line
                      :id (new-id svg-file 'line-ids)
                      :x1 gxoffs
                      :x2 (+ gxoffs (* xscale end-time))
                      :y1 (+ gyoffs ypos)
                      :y2 (+ gyoffs ypos)
                      :stroke-width 0.2
                      :fill-color stroke-color
                      :stroke-color stroke-color)
       (if (nth 0 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 60
                           :length 10
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 1 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 30
                           :non-drawing-modulo 2
                           :length 8
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 2 ticks)
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 10
                           :non-drawing-modulo 3
                           :length 5
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 3 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 5
                           :non-drawing-modulo 2
                           :length 3
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos))
       (if (nth 4 ticks) 
           (get-tick-lines end-time svg-file
                           :color stroke-color
                           :interval 1
                           :non-drawing-modulo 5
                           :length 1
                           :xscale xscale
                           :gxoffs gxoffs
                           :gyoffs gyoffs
                           :ypos ypos)))
      (add-elements
       (make-instance 'svg-group
                      :id (new-id svg-file 'group-ids))
       (get-time-text end-time svg-file
                      :color stroke-color
                      :xscale xscale
                      :gxoffs (- gxoffs 7.5)
                      :gyoffs gyoffs
                      :ypos (- ypos 2)))))))

#|

;;; no grouping:

(defun svg-zeitachse 
    (end-time svg-file &key 
     (ypos -50)
     (gxoffs 20)
     (gyoffs 300)
     (xscale 1)
     (ticks '(t t t t t)))
  (append
   (list
    (make-instance 'svg-layer 
                   :name "Zeitachse" 
                   :id (new-id svg-file 'layer-ids))
    (make-instance 'svg-line
                   :id (new-id svg-file 'line-ids)
                   :x1 gxoffs
                   :x2 (+ gxoffs (* xscale end-time))
                   :y1 (+ gyoffs ypos)
                   :y2 (+ gyoffs ypos)
                   :stroke-width 0.2
                   :fill-color +black+
                   :stroke-color +black+))
   (if (nth 0 ticks) 
       (get-tick-lines end-time svg-file 
		       :interval 60
		       :length 10
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 1 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 30
		       :non-drawing-modulo 2
		       :length 8
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 2 ticks)
       (get-tick-lines end-time svg-file
		       :interval 10
		       :non-drawing-modulo 3
		       :length 5
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 3 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 5
		       :non-drawing-modulo 2
		       :length 3
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (if (nth 4 ticks) 
       (get-tick-lines end-time svg-file
		       :interval 1
		       :non-drawing-modulo 5
		       :length 1
		       :xscale xscale
		       :gxoffs gxoffs
		       :gyoffs gyoffs
		       :ypos ypos))
   (cons
    (get-time-text end-time svg-file
                   :xscale xscale
                   :gxoffs (- gxoffs 7.5)
                   :gyoffs gyoffs
                   :ypos (- ypos 2)))))
|#

(defun get-time-text (end-time svg-file
                      &key
                        (color "#000000")
                        (start 0)
                        (font-family "Arial")
                        (font-size 6)
                        (font-style "normal")
                        (ypos 0) (gxoffs 20) (gyoffs 300) (xscale 1))
  (loop for time from start to end-time by 30
     append 
     (list (make-instance 'svg-text
			  :id (new-id svg-file 'text-ids)
			  :x (+ gxoffs (* xscale (- time start)))
			  :y (+ gyoffs ypos)
			  :font-family font-family
			  :font-size font-size
			  :font-style font-style
			  :label (format nil "~2,'0d:~2,'0d"
					 (round (floor time 60))
					 (mod time 60))
			  :stroke-width 0.2
			  :fill-color color
			  :stroke-color color))))


;; Original quo colors

(defparameter *svg-colors*
      (let ((hash (make-hash-table :test #'equal)))
        (mapcar
         (lambda (l) (setf (gethash (first l) hash) (second l))
           (setf (gethash (second l) hash) (first l)))
         '((0 "#000000")
	   (1 "#0000FF") (2 "#FF0000") (3 "#00FFFF") (4 "#00FF00")
	   (5 "#FFB400") (6 "#FF00FF") (7 "#FFFFFF") (8 "#00008F") (9 "#0000B0")
	   (10 "#0000D1") (11 "#87CFFF") (12 "#008F00") (13 "#00B000") (14 "#00D100")
	   (15 "#008F8F") (16 "#00B0B0") (17 "#00D1D1") (18 "#8F0000") (19 "#B00000")
	   (20 "#D10000") (21 "#8F008F") (22 "#B000B0") (23 "#D100D1") (24 "#803000")
	   (25 "#A14000") (26 "#BF6100") (27 "#FF8080") (28 "#FFA1A1") (29 "#FFBFBF")
	   (30 "#FFE0E0") (31 "#FFD600") (32 "#FF334C") (33 "#CCCCCC") (34 "#999999")
	   (35 "#666666") (36 "#333333") (37 "#00E500") (38 "#007FFF") (39 "#CC7F19")
	   (40 "#8C19FF") (41 "#FFD5DA") (42 "#A5E5A5") (43 "#A0D0FF") (44 "#CCA570")
	   (45 "#CF9EFF") (46 "#CF9EFF")))
        hash))

(defun color-lookup (coloridx &optional (color-hash *svg-colors*))
  (gethash coloridx color-hash))

(defparameter *pd-colors*
  (let ((hash (make-hash-table)))
    (mapcar (lambda (l) (setf (gethash (first l) hash) (second l)))

            '((888 0) (555 1) (900000 2) (77 3) (80 4) (959452 5) (43004 6) (673356 7)
              (99 8) (909 9) (9 10) (77 11) (758 12) (1009 13) (821 14) (676 15) (115 16)
              (1724 17) (1652 18) (202 19) (1201 20) (591 21) (1670 22) (1428 23) (1789 24)
              (1272 25) (555 26) (573 27) (934 28) (1042 29) (542 30) (148 31) (1912 32)
              (1855 33) (1269 34) (1140 35) (286 36) (1785 37) (271 38) (331 39) (1069 40)
              (530 41) (1577 42) (593 43) (734 44) (1649 45) (1240 46) (270 47) (324 48)
              (1782 49) (1096 50) (555 51) (1839 52) (536 53) (532 54) (1285 55) (1029 56)
              (934 57) (12 58) (48 59) (801 60) (1195 61) (1632 62) (510 63) (1371 64)
              (98 65) (44 66) (13 67) (1172 68) (1466 69) (544 70) (1944 71) (87 72)
              (1756 73) (614 74) (1247 75) (1048 76) (1271 77) (601 78) (51 79) (425 80)
              (1692 81) (560 82) (207 83) (324 84) (1110 85) (944 86) (1226 87) (1748 88)
              (1175 89) (301 90) (1329 91) (702 92) (1605 93) (164 94) (1705 95) (308 96)
              (1255 97) (1125 98) (458 99) (1383 100) (551 101) (1530 102) (736 103)
              (809 104) (870 105) (440 106) (847 107) (1179 108) (758 109) (31 110)))
    hash))

(defparameter *pd-colors-reverse*
  (let ((hash (make-hash-table)))
    (mapcar (lambda (l) (setf (gethash (second l) hash) (first l)))
            '((888 0) (555 1) (900000 2) (77 3) (80 4) (959452 5) (43004 6) (673356 7)
              (99 8) (909 9) (9 10) (77 11) (758 12) (1009 13) (821 14) (676 15) (115 16)
              (1724 17) (1652 18) (202 19) (1201 20) (591 21) (1670 22) (1428 23) (1789 24)
              (1272 25) (555 26) (573 27) (934 28) (1042 29) (542 30) (148 31) (1912 32)
              (1855 33) (1269 34) (1140 35) (286 36) (1785 37) (271 38) (331 39) (1069 40)
              (530 41) (1577 42) (593 43) (734 44) (1649 45) (1240 46) (270 47) (324 48)
              (1782 49) (1096 50) (555 51) (1839 52) (536 53) (532 54) (1285 55) (1029 56)
              (934 57) (12 58) (48 59) (801 60) (1195 61) (1632 62) (510 63) (1371 64)
              (98 65) (44 66) (13 67) (1172 68) (1466 69) (544 70) (1944 71) (87 72)
              (1756 73) (614 74) (1247 75) (1048 76) (1271 77) (601 78) (51 79) (425 80)
              (1692 81) (560 82) (207 83) (324 84) (1110 85) (944 86) (1226 87) (1748 88)
              (1175 89) (301 90) (1329 91) (702 92) (1605 93) (164 94) (1705 95) (308 96)
              (1255 97) (1125 98) (458 99) (1383 100) (551 101) (1530 102) (736 103)
              (809 104) (870 105) (440 106) (847 107) (1179 108) (758 109) (31 110)))
    hash))

(defun pd-color->svg-color (pd-color)
  (or (color-lookup (color-lookup pd-color *pd-colors*) *svg-colors*)
      "#000000"))

(defun svg-color->pd-color (svg-color)
  (or (color-lookup (color-lookup svg-color *svg-colors*) *pd-colors-reverse*)
      (mod (sxhash svg-color) 111)))

;;; (pd-color->svg-color 2) -> "#87CFFF"

;;; (svg-color->pd-color "#87CFFF") -> 77

;;; (pd-color->svg-color 758) -> nil

(defun opacity->db (opacity)
  (- (* opacity 60) 60))

(defun db->opacity (db)
  (max (min 1.0 (+ (/ db 60) 1)) 0.0))
