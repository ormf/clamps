;;; 
;;; cl-sfz.lisp
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

(defparameter *sfz-tables* (make-hash-table))

(defparameter keynames '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(defun current-date ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
    (declare (ignorable second minute hour date month year day-of-week dst-p tz))
    (format nil "~2,'0d.~2,'0d.~4,'0d" date month year)))

(defparameter *sfz-header*
  "//  ***** ~a.sfz *****
//
//  Created ~a
//  By Orm Finnendahl
//  Flute played by Jaume Darbra Fa

")

;;; (import '(lsample-play sample-play) 'incudine)

;;; (declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt 2 (/ steps 12)))

(defun line->plist (line)
  "convert all key=val pairs in line into a plist with alternating :key val entries.
sample paths and key names are converted to linux/cl
conventions. Returns the plist."
  (let ((pairs (split "\\s+" line)))
    (loop for keyval in pairs
          append (destructuring-bind (key value)
                     (split "\\s+" (regex-replace "=" keyval " "))
                   (cond
                     ((member (string-upcase key) '("LOKEY" "HIKEY" "PITCH_KEYCENTER") :test #'string=)
                      (list (intern (string-upcase (regex-replace "_" key "-")) 'keyword)
                            (val->keynum value)))
                     ((string= key "sample")
                      (list :sample (regex-replace "\\" value "/")))
                     (t (list (intern (string-upcase (regex-replace "_" key "-")) 'keyword)
                              (read-from-string value))))))))

(defun val->keynum (str)
  (let ((value (read-from-string str)))
    (cond
      ((symbolp value) (+ (position (read-from-string (string-upcase (subseq str 0 (1- (length str)))))
                                    keynames :test #'string=)
                          (* 12 (1+ (read-from-string (subseq str (1- (length str))))))))
      (:else value))))

(defun skip-to-next-region (in)
  (loop
    for line = (read-line in nil nil)
;;;    do (break "line: ~S" line)
    while line
    until (string= (string-left-trim '(#\SPACE #\TAB) line)  "<region>")
    finally (return line)))

(defun parse-region (in)
    (loop
      for line = (read-line in nil nil)
;;;      do (break "line: ~S" line)
      while line
      until (string= (string-left-trim '(#\SPACE #\TAB) line) "")
      append (if line (line->plist line))))

(defun keynum->pitch2 (keynum)
  (multiple-value-bind (oct pc) (floor keynum 12)
    (format nil "~a~d"
            (aref #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") pc)
            (1- oct))))

(defun sfz-region (name keynum lo hi tune volume)
  (format nil
    "<region>~%sample=~a~%volume=~a~%lokey=~d~%hikey=~d~%tune=~d~%pitch_keycenter=~a~%"
    name volume lo hi tune (keynum->pitch2 keynum)))

(defun get-sfz-attributes (entry &rest keys)
  (mapcar (lambda (key) (getf entry key))
          keys))

(defun entry->region (entry)
  (apply #'sfz-region (get-sfz-attributes entry :sample :pitch-keycenter :lokey :hikey :tune :volume)))

(defun parse-sfz (file)
  "parse all regions in file to plists and return them in a list."
  (with-open-file (in file)
    (loop
      while (skip-to-next-region in)
      collect (parse-region in) into result
      finally (return result))))

;;; (parse-sfz "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz")

(defun random-elem (seq)
  "return a random element of seq."
  (elt seq (random (length seq))))

(defun push-keynums (sample-def keynum-array)
  "push the sample-def onto the keynum-array's elements at all indexes
from lokey to hikey of the sample def."
  (loop for keynum from (or (getf sample-def :lokey) 0) to (or (getf sample-def :hikey) 127)
        do (push (getf sample-def :lsample) (aref keynum-array keynum))))

(defun get-keynum-array (file &key play-fn)
  "push all sample-defs in file to a new array. Its 128
elems (representing keynums) contain the sample-defs for the
respective keynum. Overlapping key-ranges are represented by a list of
all applicable sample-defs at the keynum's array-index."
  (let ((keynum-array (make-array 128 :adjustable nil :element-type 'list :initial-element nil))
        (sfz-file-path (pathname file)))
    (dolist (entry (reverse (remove nil (parse-sfz file))))
      (setf (getf entry :lsample) (oid:sfz->lsample entry sfz-file-path :play-fn play-fn))
      (push-keynums entry keynum-array))
    keynum-array))

(defun sfz-get-range (file)
  (let ((keynums (mapcar #'of-incudine-dsps:get-keynum (parse-sfz file))))
    (list (round (apply #'min keynums)) (round (apply #'max keynums)))))

(defun sf-table-get-range (preset)
  (if (gethash preset *sfz-tables*)
      (let ((keynums (loop for slist across (gethash preset *sfz-tables*)
                           append (mapcar (lambda (x) (round (of-incudine-dsps:lsample-keynum x))) slist))))
        (list (apply #'min keynums) (apply #'max keynums)))))

;;; (sf-table-get-range :altoflute-k)

(defun get-scale (keynum sample-data)
  "calc time scaling factor from target pitch and sample-data."
  (expt 2 (/ (- keynum
                (+ (getf sample-data :pitch-keycenter)
                   (/ (or (getf sample-data :tune) 0) 100.0)))
             12)))

(defun load-sfz-preset (file name &key force (play-fn #'play-sfz-loop))
  "load sfz file into a preset with the id name. In case this preset
already exists, the old one will only be overwritten if :force is set
to t."
  (when (or force (not (gethash name *sfz-tables*)))
    (format t "loading ~S from ~a~%" name file)
    (setf (gethash name *sfz-tables*)
          (get-keynum-array file :play-fn play-fn))))

;;; (load-sfz-preset "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz" :flute-nv)

(defun list-sfz-presets (&key (loaded nil))
  "return a sorted list of all registered sfz-presets."
  (if loaded (sort (loop for k being each hash-key of *sfz-tables* collect k) #'string<)
      (sort (loop for k being each hash-key of cl-user:*sfz-preset-lookup* collect k) #'string<)))

(defun remove-sfz-preset (name)
  (remhash name *sfz-tables*))

(defun db->amp (db)
  (expt 10 (/ db 20)))

(defun sfz->lsample (sfz-entry dir &key (play-fn #'of-incudine-dsps:play-lsample*))
  (let* ((abs-filepath (of-incudine-dsps:abs-path (getf sfz-entry :sample) dir))
         (buffer (of-buffer-load abs-filepath)))
    (of-incudine-dsps::make-lsample
     :filename abs-filepath
     :buffer buffer
     :play-fn play-fn
     :keynum (of-incudine-dsps:get-keynum sfz-entry)
     :amp (incudine::db->linear (getf sfz-entry :volume 0))
     :loopstart (incudine::sample (or (getf sfz-entry :loop-start) 0))
     :loopend (incudine::sample (or (getf sfz-entry :loop-end) (buffer-frames buffer))))))

#|
(defun play-sample (pitch db dur &key (pan 0.5) (preset :flute-nv) (sfz-tables *sfz-tables*) (startpos 0))
  (let* ((map (gethash preset sfz-tables))
         (sample (random-elem (aref map (round pitch))))
         (amp (of-incudine-dsps:lsample-amp sample))
         (play-fn (of-incudine-dsps:lsample-play-fn sample))
         (rate (incudine::sample (ct->fv (- pitch (of-incudine-dsps:lsample-keynum sample)))))
         (buffer (of-incudine-dsps:lsample-buffer sample)))
;;    (break "~a" (eql play-fn #'sample-play))
    (cond
      ((eql play-fn #'lsample-play)
       (of-incudine-dsps:lsample-play buffer dur (db->amp db) rate pan
                               (of-incudine-dsps:lsample-loopstart sample)
                               (of-incudine-dsps:lsample-loopend sample)
                               startpos))
      ((eql play-fn #'sample-play)
       (of-incudine-dsps:sample-play buffer dur (* amp (db->amp db)) rate pan
                              startpos))
      (t
       (error "play-fn not found: ~a" play-fn)))))
|#



(defun add-sfz-preset (preset file)
  "add preset to file association to 'cl-user:*sfz-preset-lookup*."
  (setf (gethash preset cl-user:*sfz-preset-lookup*) file))

(defun get-sfz-preset (preset)
  (or
   (gethash preset *sfz-tables*)
   (and
    (boundp 'cl-user:*sfz-preset-lookup*)
    (boundp 'cl-user:*sfz-preset-path*)
    (let* ((name (gethash preset cl-user:*sfz-preset-lookup*))
           (sfz-preset-file (and name
                                 (incudine-bufs:get-sndfile-path
                                  name
                                  cl-user:*sfz-preset-path*))))
      (and sfz-preset-file (load-sfz-preset sfz-preset-file preset)
           (gethash preset *sfz-tables*))))
   (warn "preset ~s not found!" preset)))

(setf (fdefinition 'ensure-sfz-preset) #'get-sfz-preset)

(defun play-sfz (pitch db dur &key (pan 0.5) (preset :flute-nv) (startpos 0) (out1 0) out2)
  "general function: Plays sample looping or one-shot depending on the
'play-fn slot in the lsample definition."
  (let ((map (get-sfz-preset preset)))
    (if map 
        (let* ((out2 (or out2 (mod (1+ out1) 8)))
               (sample (random-elem (aref map (round pitch))))
               (buffer (of-incudine-dsps:lsample-buffer sample))
               (rate (incudine::sample (ct->fv (- pitch (of-incudine-dsps:lsample-keynum sample)))))
               (play-fn (of-incudine-dsps:lsample-play-fn sample))
               (amp (of-incudine-dsps:lsample-amp sample)))
          (cond
            ((eql play-fn #'play-sfz-loop)
             (of-incudine-dsps:play-lsample* buffer of-incudine-dsps:*env1* dur (+ amp db) rate pan
                                                (of-incudine-dsps:lsample-loopstart sample)
                                                (of-incudine-dsps:lsample-loopend sample) startpos out1 out2
                                                :head 200)
             )
            (t
             (of-incudine-dsps:play-lsample* buffer of-incudine-dsps:*env1* dur (+ amp db) rate pan startpos out1 out2
                                             :head 200) ;;
             )))
        (error "preset ~S not found!" preset))))

;;; (play-sfz 60 0 1 :pan 0 :out1 0)
#|
(let* ((sample (first (aref (gethash :flute-nv *sfz-tables*) 60)))
       (args (list (lsample-buffer sample) 1 1 1 0.5 (lsample-loopstart sample)
                   (lsample-loopend sample))))
  (apply #'lsample-play args)
  args)

(incudine::play-buffer-stretch* (lsample-buffer (first (aref (gethash :flute-nv *sfz-tables*) 60))))
|#

(defun play-sfz-loop (pitch db dur &key (pan 0.5) (preset :flute-nv) (startpos 0) (out1 0) out2)
  "Plays sample looping independent of the 'play-fn slot in the lsample definition."
  (let ((map (get-sfz-preset preset)))
    (if map
        (let* ((sample (random-elem (aref map (round pitch))))
               (out2 (or out2 (mod (1+ out1) 8)))
               (buffer (of-incudine-dsps:lsample-buffer sample))
               (rate (incudine::sample (ct->fv (- pitch (of-incudine-dsps:lsample-keynum sample)))))
               (amp (of-incudine-dsps:lsample-amp sample))
               (loopstart (of-incudine-dsps:lsample-loopstart sample))
               (loopend (of-incudine-dsps:lsample-loopend sample)))
          (of-incudine-dsps:play-lsample* buffer of-incudine-dsps:*env1* dur (+ amp db) rate pan loopstart loopend startpos out1 out2
                                          :head 200)
          (error "preset ~S not found!" preset)))))

;;; (play-sfz-loop 60 0 10 :pan 0 :out1 1)

(defun play-sfz-one-shot (pitch db dur &key (pan 0.5) (preset :flute-nv) (startpos 0) (out1 0) out2)
  "Plays sample once independent of the 'play-fn slot in the lsample definition."
  (let ((map (get-sfz-preset preset)))
    (if map
        (let* ((sample (random-elem (aref map (round pitch))))
               (out2 (or out2 (mod (1+ out1) 8)))
               (buffer (of-incudine-dsps:lsample-buffer sample))
               (rate (incudine::sample (ct->fv (- pitch (of-incudine-dsps:lsample-keynum sample)))))
               (amp (of-incudine-dsps:lsample-amp sample)))
;;;    (break "rate: ~a" rate)
          (of-incudine-dsps:play-lsample* buffer of-incudine-dsps:*env1* dur (+ amp db) rate pan startpos out1 out2
                                          :head 200))
        (error "preset ~S not found!" preset))))

;;; (play-sfz-one-shot 60 0 0.5)
