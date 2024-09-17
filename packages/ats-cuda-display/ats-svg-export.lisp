;;; 
;;; ats-svg-export.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :ats-cuda-display)

(defun calc-opacity (amp &key (brightness 20))
  (max 0.0 (min 1.0 (* brightness amp))))

(defun ftom (freq)
  (+ 69 (* 12 (log (/ freq 440) 2))))

(defun get-max-freq (arr partial frames)
  (loop
    for frame below frames
    for freq = (aref arr partial frame)
    maximize freq))

;;; (defparameter *html-src-dir* (merge-pathnames (asdf:system-relative-pathname :ats-cuda "html/")))


(defun ats->svg (ats-sound &key (brightness 20) x-scale (width 960) (height 540)
                             fname)
  "Generate a SVG file of the <ats-sound> and save it at \"/tmp/www/ats.svg\""
  (let ((svg (make-instance 'svg-ie:svg-file :fname
                            (or fname
                                (if (and (find-package :cm.svgd) (find-symbol "SVG-DIR" :cm.svgd))
                                    (pathname (format nil "~a/~a.svg"
                                                      (namestring (symbol-value (find-symbol "SVG-DIR" :cm.svgd)))
                                                      (string-downcase (ats-sound-name ats-sound)))))
                                (if (find-package :ats-cuda-display)
                                    (pathname
                                     (format nil "~a/~a.svg" ats-cuda:*ats-snd-dir*
                                             (string-downcase (ats-sound-name ats-sound)))))
                                (pathname "/tmp/www/ats-snd.svg"))
                                             :width width :height height)))
    (with-slots (sampling-rate window-size frame-size frames partials frq amp frqmax)
        ats-sound
      (let* (
;;;              (duration (/ (1- window-size) sampling-rate))
             (dtime (/ frame-size sampling-rate))
             (x-scale (or x-scale (/ width (* dtime frames))))
             (maxfreq (+ 100 (get-max-freq frq (1- partials) frames))))
        (svg-ie::add-elements
         svg
         (loop for frame-idx below frames
               for time from 0 by dtime
               append (loop
                        for partial below partials
                        if (not (zerop (aref frq partial frame-idx)))
                          collect (let ((x1 (float (* x-scale time) 1.0))
                                        (y1 (+ height (* -1 height (float (/ (aref frq partial frame-idx) maxfreq) 1.0))))
                                        (y2 (+ height (* -1 height (float (/ (aref frq partial (min (1+ frame-idx) (1- frames))) maxfreq) 1.0))))
                                        (width (float (* x-scale dtime) 1.0))
                                        (color "#2255FF")
                                        (opacity (calc-opacity (float (aref amp partial frame-idx) 1.0) :brightness brightness)))
;;;                                    (format t "opacity: ~a, amp: ~a~%" opacity (float (amp sinoid) 1.0))
                                    (make-instance 'svg-ie::svg-line :x1 x1 :y1 y1
                                                                     :x2 (+ x1 width) :y2 y2
;;;                                                                    :stroke-width stroke-width
                                                                     :opacity opacity
                                                                     :stroke-color color 
                                                                     ;; :fill-color color
                                                                     :id (svg-ie:new-id svg 'line-ids))))))))
    (svg-ie:export-svg-file svg :showgrid nil)))

;;; (ats->svg cl :brightness 100)

