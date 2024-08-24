;;; svg-export-examples.lisp
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package :svg-import-export)


(defparameter *test* nil)

;;; example code to create an svg file with 8 squares and a time axis
;;; above. The file is stored in "/tmp/test.svg".

(progn
  (setf *test*
        (let ((end-time 100)
              (xscale 10)
              (gxoffs 20)
              (svg-file (make-instance 'svg-file)))
          (setf (slot-value svg-file 'elements) 
                (list
                 (append (list 
                          (make-instance 'svg-layer 
                                         :name "Ebene 1" 
                                         :id (new-id svg-file 'layer-ids)))
                         (loop for x from 0 to end-time by 12
                            collect (make-instance 'svg-rect
                                                   :x (+ gxoffs (* xscale x)) 
                                                   :y 200
                                                   :width (* xscale 10)
                                                   :height 100
                                                   :fill-color "#333333"
                                                   :id (new-id svg-file 'rect-ids))))))
          (push (svg-zeitachse end-time svg-file
                               :gxoffs gxoffs :gyoffs 100 :xscale xscale)
                (elements svg-file))
          svg-file))
  (export-svg-file *test*))

;;; example for printing a svg Textobject as svg XML to a stream:

(print-to-stream (make-instance 'svg-text) t)

;;; The elements of the svg file from above:

(elements *test*)

;;; push a piano-roll layer to *test*

(push (svg-piano-roll *test*) (elements *test*))

;;; export *test*

(export-svg-file *test*)

(with-svg-file (svg-file :fname "/tmp/test.svg" :showgrid nil)
  (let ((end-time 100)
        (xscale 10)
        (gxoffs 20))
    (setf (elements svg-file) 
          (list
           (append (list 
                    (make-instance 'svg-layer 
                                   :name "Ebene 1" 
                                   :id (new-id svg-file 'layer-ids)))
                   (loop for x from 0 to end-time by 12
                         collect (make-instance 'svg-rect
                                                :x (+ gxoffs (* xscale x)) 
                                                :y 200
                                                :width (* xscale 10)
                                                :height 100
                                                :fill-color "#333333"
                                                :id (new-id svg-file 'rect-ids))))))
    (push (svg-zeitachse end-time svg-file
                         :gxoffs gxoffs :gyoffs 100 :xscale xscale)
          (elements svg-file))
    (push (svg-piano-roll svg-file)
          (elements svg-file))
    (push (svg-staff-system svg-file)
          (elements svg-file))
    (export-svg svg-file)))
