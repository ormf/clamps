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

(unless (boundp 'cl-user::*sfz-preset-lookup*)
  (defvar cl-user::*sfz-preset-lookup* (make-hash-table)))

(unless (boundp 'cl-user::*sfz-preset-lookup*)
  (defvar cl-user::*sfz-preset-path* nil))

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
//
")

;;; (import '(lsample-play sample-play) 'incudine)

;;; (declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt 2 (/ steps 12)))

(defun line->plist (line)
  "Convert all key=val pairs in line into a plist with alternating :key val entries.
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
  "Parse all regions in file to plists and return them in a list."
  (with-open-file (in file)
    (loop
      while (skip-to-next-region in)
      collect (parse-region in) into result
      finally (return result))))

;;; (parse-sfz "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz")

(defun random-elem (seq)
  "Return a random element of seq."
  (elt seq (random (length seq))))

(defun push-keynums (sample-def keynum-array)
  "push the sample-def onto the keynum-array's elements at all indexes
from lokey to hikey of the sample def."
  (loop for keynum from (or (getf sample-def :lokey) 0) to (or (getf sample-def :hikey) 127)
        do (push (getf sample-def :lsample) (aref keynum-array keynum))))

(defun get-keynum-array (file &key play-fn)
  "Push all sample-defs in sfz /file/ to a new array. Its 128
elems (representing keynums) contain the sample-defs for the
respective keynum. Overlapping key-ranges are represented by a list of
all applicable sample-defs at the keynum's array-index. Optionally set
the /:play-fn/ for all samples Returns the array.

@Arguments
file - String or Pathname of the sfz file.

:play-fn - Function denoting the play function to use for all
samples. Possible choices are:

- <<#'play-sfz-loop>>

- <<#'play-sfz-one-shot>>
"
  (let ((keynum-array (make-array 128 :adjustable nil :element-type 'list :initial-element nil))
        (sfz-file-path (pathname file)))
    (dolist (entry (reverse (remove nil (parse-sfz file))))
      (setf (getf entry :lsample) (oid:sfz->lsample entry sfz-file-path :play-fn play-fn))
      (push-keynums entry keynum-array))
    keynum-array))

(defun sfz-get-range (ref)
  "Get the keynum range of a sfz preset or a sfz file denoted by /ref/.

@Arguments
ref - String, Keynum or Symbol reference the sfz preset.

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-preset-file
sfz-preset-loaded?
"


  (typecase ref
    (symbol
     (if (gethash ref *sfz-tables*)
         (let ((keynums
                 (loop for slist across (gethash ref *sfz-tables*)
                       append (mapcar (lambda (x)
                                        (round (oid:lsample-keynum x)))
                                      slist))))
           (list (apply #'min keynums) (apply #'max keynums)))
         (error "can't find sfz preset: ~S" ref)))
    (otherwise
     (let ((keynums (mapcar #'oid:get-keynum (parse-sfz ref))))
       (list (round (apply #'min keynums)) (round (apply #'max keynums)))))))

(defun sfz-table-get-range (preset)
  "get the min and max keynum of preset."
  (if (gethash preset *sfz-tables*)
      (let ((keynums
              (loop for slist across (gethash preset *sfz-tables*)
                    append (mapcar (lambda (x)
                                     (round (oid:lsample-keynum x)))
                                   slist))))
        (list (apply #'min keynums) (apply #'max keynums)))))

;;; (sf-table-get-range :altoflute-k)

(defun get-scale (keynum sample-data)
  "calc time scaling factor from target pitch and sample-data."
  (expt 2 (/ (- keynum
                (+ (getf sample-data :pitch-keycenter)
                   (/ (or (getf sample-data :tune) 0) 100.0)))
             12)))

(defun load-sfz-preset (file name &key force (play-fn #'play-sfz-loop))
  "Load a sfz file into a preset with the id name. In case this preset
already exists, the old one will only be overwritten if force is
set to t. This function normally doesn't need to be called
explicitely. The preferred mechanism to deal with sfz presets is by
using a combination of <<add-sfz-preset>> and <<ensure-sfz-preset>>.

@Arguments
file - Path or filename of the sfz file to load

name - symbol to identify the preset (preferably a keyword, but any
symbol works)

:force - Force loading of the preset even if it already exists.

:play-fn - The play-fn to use when playing a sound. Possible choices
are:

- <<#'play-sfz-loop>>

- <<#'play-sfz-one-shot>>

@Examples

(load-sfz-preset \"~/quicklisp/local-projects/clamps/packages/cl-sfz/snd/sfz/Flute-nv/000_Flute-nv.sfz\" :flute-nv)

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-file
sfz-preset-loaded?
"
  (when (or force (not (gethash name *sfz-tables*)))
    (format t "loading ~S from ~a~%" name file)
    (setf (gethash name *sfz-tables*)
          (get-keynum-array file :play-fn play-fn))))

(defun list-sfz-presets (&key (loaded nil))
  "Return a sorted list of all sfz preset names.

@Arguments
:loaded - Boolean to indicate whether only the preset names of
loaded presets should be returned. If /nil/ all registered preset
names are returned.
"
  (if loaded (sort (loop for k being each hash-key of *sfz-tables* collect k) #'string<)
      (sort (loop for k being each hash-key of cl-user::*sfz-preset-lookup* collect k) #'string<)))

(defun sfz-preset-buffer (preset pitch)
  "return the buffer(s) of preset for pitch in a list."
  (mapcar
   #'of-incudine-dsps:lsample-buffer
   (aref (get-sfz-preset preset) (round pitch))))

(defun sfz-preset-loaded? (preset)
  "Predicate to test if sfz preset is loaded.

@Arguments
preset - Keyword or symbol of registered preset.
@Examples
#+BEGIN_SRC lisp
;;; Directly after Clamps startup:

(sfz-preset-loaded? :flute-nv) ;; => nil

(output (new sfz))
;; => loading :flute-nv from ~/quicklisp/local-procects/clamps/extra/snd/sfz/Flute-nv/000_Flute-nv.sfz
;; No values

(sfz-preset-loaded? :flute-nv) ;; => t
#+END_SRC

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-file
"
  (if (gethash preset *sfz-tables*) t))

(defun remove-sfz-preset (name)
  "Remove the soundfile map associated with name. This is the opposite of <<load-sfz-preset>>.

@Arguments
name - Keyword or symbol of the registered preset.
@Note
The soundfile buffers of the samples used in the sfz description
and the association between the preset name and the sfz file are
*not* removed! Only the association between the preset name, the
keynums and the buffers are removed.

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-file
sfz-preset-loaded?
"  (remhash name *sfz-tables*))

(defun db->amp (db)
  (expt 10 (/ db 20)))

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
  "Register the association between a sfz preset name /key/ and the
/filename/ of its /.sfz/ file. The filename can be absolute or
relative. If relative, all folders in <<*sfz-preset-path*>> will get
recursively searched when the preset gets loaded.

@Arguments
preset - A keyword or symbol to name the preset
file - A string or path to the associated sfz file. If the file
path is relative, it will be searched recursively in all paths of
/cl-user::*sfz-file-path*/.

@Note
This function only stores the association between the preset
name and its sfz file. Loading of its samples into memory is done
implicitely when the preset is used by a playing function like
<<#'play-sfz>> or by using <<#'ensure-sfz-preset>> or its synonym
<<#'get-sfz-preset>>.

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-file
sfz-preset-loaded?
"
  (setf (gethash preset cl-user::*sfz-preset-lookup*) file))

(defun sfz-preset-file (preset)
  "Return the full path of /preset/.

@Arguments
preset - Keyword or symbol of a registered sfz preset.

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-loaded?
"
  (and
   (boundp 'cl-user::*sfz-preset-lookup*)
   (boundp 'cl-user::*sfz-preset-path*)
   (let ((name (gethash preset cl-user::*sfz-preset-lookup*)))
     (and name
          (incudine-bufs:get-sndfile-path
           name
           cl-user::*sfz-preset-path*)))))

(defun get-sfz-preset (preset &key force (play-fn  #'play-sfz-loop))
  "Load the sfz definition of /preset/ and all its samples into the
system if it hasn't been loaded previously. If force is /t/, force
reload. Optionally set a play function.

The association between the preset name and its sfz file has to be
established before using <<add-sfz-preset>>, otherwise a warning is
issued and no file is loaded.

@Arguments
preset - A keynum or symbol to serve as the name/id of the preset.

:force - A boolean indicating to force a reload even if the preset
has been loaded before.

:play-fn - The play function to be used. Possible options are:

- <<#'play-sfz-loop>>

- <<#'play-sfz-one-shot>>

@See-also
add-sfz-preset
ensure-sfz-preset
get-sfz-preset
load-sfz-preset
remove-sfz-preset
sfz
sfz-get-range
sfz-preset-file
sfz-preset-loaded?
"
  (or
   (and (not force) (gethash preset *sfz-tables*))
   (let ((sfz-preset-file (sfz-preset-file preset)))
     (if sfz-preset-file
         (and (load-sfz-preset sfz-preset-file preset
                               :force force :play-fn play-fn)
              (gethash preset *sfz-tables*))
         (warn "preset ~s not found!" preset)))))

(setf (fdefinition 'ensure-sfz-preset) #'get-sfz-preset)

(defun play-sfz (pitch db dur &key (pan 0.5) (preset :flute-nv) (startpos 0) (out1 0) out2)
  "Play a sfz preset with stereo panning to incudine's audio outputs
or a bus using the /play-fn/ of the sample to be played.

@Arguments
pitch - Pitch in Midicent.
db - Amplitude in dB. /0/ corresponds to a
scaling factor of /1/, /-100/ to a scaling factor of /0/.

dur - Duration in seconds.
:pan - Number in the range /[0..1]/ defining equal power panning
between /out0/ and /out1/.

:preset - The name of a registered preset. If the preset hasn't been
loaded it will get loaded before playback starts.

:startpos - The startposition in the sample in seconds.
:out1 - Zero based index of the first outlet.
:out2 - Zero based index of the second outlet. If not specified, /(mod (1+ out1) 8)/ will be used.

@See-also
play-sfz-loop
play-sfz-one-shot
"
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
  "Play a sfz preset with stereo panning to incudine's audio outputs
or a bus. Loop the sound according to the loop settings of the
sample in the sfz file or loop the whole sound if not present. This
function always uses loop playback regardless of the setting of
/play-fn/ in the sample to be played.


@Arguments
pitch - Pitch in Midicent.

db - Amplitude in dB. /0/ corresponds to a scaling factor of /1/,
/-100/ to a scaling factor of /0/.

dur - Duration in seconds.

:pan - Number in the range /[0..1]/ defining equal power panning
between /out0/ and /out1/.

:preset - The name of a registered preset. If the preset hasn't been
loaded it will get loaded before playback starts.

:startpos - The startposition in the sample in seconds.

:out1 - Zero based index of the first outlet.

:out2 - Zero based index of the second outlet. If not specified, /(mod (1+ out1) 8)/ will be used.

@See-also
play-sfz
play-sfz-one-shot
"
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
  "Play a sfz preset with stereo panning to incudine's audio outputs
or a bus once (regardless of the setting of /play-fn/ in the sample to
be played). Playback stops after /dur/ seconds or at the end of the
sample, if /dur/ is longer than the length of the sample.

@Arguments
pitch - Pitch in Midicent.
db - Amplitude in dB. /0/ corresponds to a
scaling factor of /1/, /-100/ to a scaling factor of /0/.

dur - Duration in seconds.
:pan - Number in the range /[0..1]/ defining equal power panning
between /out0/ and /out1/.

:preset - The name of a registered preset. If the preset hasn't been
loaded it will get loaded before playback starts.

:startpos - The startposition in the sample in seconds.
:out1 - Zero based index of the first outlet.
:out2 - Zero based index of the second outlet. If not specified, /(mod (1+ out1) 8)/ will be used.

@See-also
play-sfz
play-sfz-loop
"
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
