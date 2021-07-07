;;; 
;;; sfz.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-sfz)

(setf *print-case* :downcase)

(defparameter *sf-tables* (make-hash-table))

;;; (declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt 2 (/ steps 12)))

(defun skip-to-next-region (in)
  (loop
    for line = (read-line in nil nil)
    while line
    until (string= line "<region>")
    finally (return line)))

(defun line->plist (line)
  "convert all key=val pairs in line into a plist with alternating :key val entries.
sample paths and key names are converted to linux/cl
conventions. Returns the plist."
  (let ((pairs (split "\\s+" line)))
    (loop for keyval in pairs
          append (destructuring-bind (key value)
                     (split "\\s+" (regex-replace "=" keyval " "))
                   (cond
                     ((string= key "sample")
                      (list :sample (regex-replace "\\" value "/")))
                     (t (list (intern (string-upcase (regex-replace "_" key "-")) 'keyword)
                              (read-from-string value))))))))

(defun parse-region (in)
    (loop
      for line = (read-line in nil nil)
      while line
      until (string= line "")
      append (line->plist line)))

(defun parse-sfz (file)
  "parse all regions in file to plists and return them in a list."
  (with-open-file (in file)
    (loop
      while (skip-to-next-region in)
      for region = (parse-region in)
      collect region into result
      finally (return result))))

;;; (parse-sfz "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz")


(defun random-elem (seq)
  "return a random element of seq."
  (elt seq (random (length seq))))

(defun push-keynums (sample-def keynum-array)
  "push the sample-def onto the keynum-array's elements at all indexes
from lokey to hikey of the sample def."
  (loop for keynum from (getf sample-def :lokey) to (getf sample-def :hikey)
        do (push (getf sample-def :lsample) (aref keynum-array keynum))))

(defun get-keynum-array (file)
  "push all sample-defs in file to a new array. Its 128
elems (representing keynums) contain the sample-defs for the
respective keynum. Overlapping key-ranges are represented by a list of
all applicable sample-defs at the keynum's array-index."
  (let ((keynum-array (make-array 128 :adjustable nil :element-type 'list :initial-element nil))
        (sfz-file-path (pathname file)))
    (dolist (entry (parse-sfz file))
      (setf (getf entry :lsample) (incudine::sfz->lsample entry sfz-file-path))
      (push-keynums entry keynum-array))
    keynum-array))

(defun get-scale (keynum sample-data)
  "calc time scaling factor from target pitch and sample-data."
  (expt 2 (/ (- keynum
                (+ (getf sample-data :pitch-keycenter)
                   (/ (or (getf sample-data :tune) 0) 100.0)))
             12)))

(defun load-sfz (file name)
  "load sfz file into a preset with the id name. In case this preset
already exists, the old one will be overwritten."
  (setf (gethash name *sf-tables*)
        (get-keynum-array file))
  name)

;;; (load-sfz "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz" :flute-nv)

(defun db->amp (db)
  (expt 10 (/ db 20)))

(defun play-lsample (pitch db dur &key (pan 0.5) (preset :flute-nv) (sf-tables *sf-tables*))
  (let* ((map (gethash preset sf-tables))
         (sample (random-elem (aref map (round pitch))))
         (rate (incudine::sample (ct->fv (- pitch (incudine::lsample-keynum sample)))))
         (buffer (incudine::lsample-buffer sample))
         (loopstart (incudine::lsample-loopstart sample))
         (loopend (incudine::lsample-loopend sample)))
    (incudine::lsample-play buffer dur (db->amp db) rate pan loopstart loopend)))

;;; (play-lsample 73.3 -6 1 :pan 0.5)

(in-package :incudine)

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

(define-vug buffer-loop-play ((buffer buffer) rate start-pos
                              loopstart loopend)
  (buffer-read buffer (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))


(defun abs-path (sample-path sfz-file-path)
  (merge-pathnames sample-path sfz-file-path))

(defun load-sample (entry dir)
  (buffer-load (abs-path (getf entry :sample) dir)))

(defun get-keynum (entry)
  (sample (+ (getf entry :pitch-keycenter) (/ (or (getf entry :tune) 0) 100))))

#|
(get-keynum '(:sample "samples/97-Flute.nv.ff.Db7.wav" :volume 3 :lokey 97 :hikey 127
  :pitch-keycenter 97 :tune -39 :offset 0 :end 118825 :loop-start 94748
  :loop-end 95657))
|#

(defstruct lsample
  "structure for a sample with two loop-points. The structure also
contains a slot for the sample buffer data."
  filename
  buffer
  (keynum +sample-zero+ :type sample)
  (loopstart +sample-zero+ :type sample)
  (loopend +sample-zero+ :type sample))

(defun sfz->lsample (sfz-entry dir)
  (let ((abs-filepath (abs-path (getf sfz-entry :sample) dir)))
    (make-lsample
     :filename abs-filepath
     :buffer (buffer-load abs-filepath)
     :keynum (get-keynum sfz-entry)
     :loopstart (sample (getf sfz-entry :loop-start))
     :loopend (sample (getf sfz-entry :loop-end)))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample 440.0d0) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(defun collect-props (names plist)
  (loop for name in names
        collect `(,name (getf ,plist ,(alexandria:make-keyword name)))))

(defmacro with-props ((&rest names) plist &rest body)
  `(let ,(collect-props names plist)
     ,@body))

#|
(defun make-keyword (symbol)
  (intern (string-upcase (symbol-name symbol)) 'keyword))

(let ((entry '(:lsample "a" :lokey 4)))
  (with-props (lsample lokey) entry

              (list lsample lokey)
              ))
|#

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))

#|
(define-ugen phasor* frame (freq init)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor freq init)))
    frm))
|#

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

(define-vug buffer-loop-play ((buffer buffer) rate start-pos
                              loopstart loopend)
  (buffer-read buffer (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(dsp! lsample-play ((buffer buffer) dur amp rate pan loopstart loopend)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 1 1 1 0.5 0 44100)
  (foreach-channel
    (cout
     (pan2
      (* amp 
	 (envelope *env1* 1 dur #'free)
	 (buffer-loop-play buffer rate 0 loopstart loopend))
      pan))))


#|
;; examples: 

 (cl-sfz:play-lsample (+ 50 (random 30)) 3 0.5)


 (loop
   for x from 1 to 200
   for time = (now) then (+ time (* *sample-rate* (random 0.05)))
   for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
   do (at time #'cl-sfz:play-lsample (+ 70 (random 20.0)) 2 amp))

bouncing to disk:

 (bounce-to-disk ("/tmp/test.wav" :pad 2)
  (loop
     for x from 1 to 200
     for time = (now) then (+ time (* *sample-rate* (random 0.05)))
     for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
     do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp)))
|#
