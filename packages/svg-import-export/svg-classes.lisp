;;; svg-classes.lisp
;;;
;;; class definitions and their print methods.
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package #:svg-import-export)

(defparameter *svg-header* "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->
<svg
   style=\"background-color:~a\"
   width=\"~dpx\"
   height=\"~dpx\"
   viewBox=\"0 0 ~d ~d\"
   id=\"svg2392\"
   inkscape:document-unit=\"px\"
   sodipodi:version=\"0.32\"
   inkscape:version=\"0.92+devel 15617 custom\"
   sodipodi:docname=\"test.svg\"
   inkscape:output_extension=\"org.inkscape.output.svg.inkscape\"
   version=\"1.1\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\">

  <defs
     id=\"defs2394\">
    <marker
       inkscape:stockid=\"DotS\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"DotS\"
       style=\"overflow:visible\"
       inkscape:isstock=\"true\">
      <path
         id=\"path6619\"
         d=\"M -2.5,-1.0 C -2.5,1.7600000 -4.7400000,4.0 -7.5,4.0 C -10.260000,4.0 -12.5,1.7600000 -12.5,-1.0 C -12.5,-3.7600000 -10.260000,-6.0 -7.5,-6.0 C -4.7400000,-6.0 -2.5,-3.7600000 -2.5,-1.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1\"
         transform=\"scale(0.2) translate(7.4, 1)\" />
    </marker>
    <marker
       inkscape:stockid=\"Arrow1Mend\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"Arrow1Mend\"
       style=\"overflow:visible;\"
       inkscape:isstock=\"true\">
      <path
         id=\"path4186\"
         d=\"M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1pt;stroke-opacity:1;fill:#000000;fill-opacity:1\"
         transform=\"scale(0.4) rotate(180) translate(10,0)\" />
    </marker>
    <marker
       inkscape:stockid=\"Arrow1Lend\"
       orient=\"auto\"
       refY=\"0.0\"
       refX=\"0.0\"
       id=\"Arrow1Lend\"
       style=\"overflow:visible;\">
      <path
         id=\"path3225\"
         d=\"M 0.0,0.0 L 5.0,-5.0 L -12.5,0.0 L 5.0,5.0 L 0.0,0.0 z \"
         style=\"fill-rule:evenodd;stroke:#000000;stroke-width:1.0pt;marker-start:none;\"
         transform=\"scale(0.8) rotate(180) translate(12.5,0)\" />
    </marker>
    <inkscape:perspective
       sodipodi:type=\"inkscape:persp3d\"
       inkscape:vp_x=\"0 : 372.04724 : 1\"
       inkscape:vp_y=\"0 : 1000 : 0\"
       inkscape:vp_z=\"1052.3622 : 372.04724 : 1\"
       inkscape:persp3d-origin=\"526.18109 : 248.03149 : 1\"
       id=\"perspective2445\" />
  </defs>
<sodipodi:namedview
     id=\"base\"
     pagecolor=\"~a\"
     bordercolor=\"666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"~3,1f\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"~a\"
     inkscape:cx=\"~a\"
     inkscape:cy=\"~a\"
     inkscape:document-units=\"px\"
     inkscape:current-layer=\"layer1\"
     inkscape:window-width=\"~a\"
     inkscape:window-height=\"~a\"
     inkscape:window-x=\"~a\"
     inkscape:window-y=\"~a\"
     showgrid=\"~a\"
     gridtolerance=\"10000\"
     inkscape:window-maximized=\"0\">
    <inkscape:grid
       type=\"xygrid\"
       id=\"default-grid-4x4\"
       spacingy=\"1\"
       spacingx=\"1\"
       dotted=\"false\" 
       empspacing=\"4\"
       enabled=\"~a\" />
    <inkscape:grid
       type=\"xygrid\"
       id=\"grid-5x5\"
       spacingy=\"1\"
       spacingx=\"1\"
       dotted=\"false\" 
       empspacing=\"5\"
       enabled=\"~a\" />
  </sodipodi:namedview>
  <metadata
     id=\"metadata2397\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
      </cc:Work>
    </rdf:RDF>
  </metadata>
")

#|

(make-class-args '(zoom cx cy w-width w-height w-x w-y))

|#

(def-exporting-class svg-file ()
  ((fname :accessor fname :initarg :fname :initform "/tmp/test.svg" :export t)
   (header :accessor header :initarg :header :initform *svg-header* :export t)
   (width  :accessor width :initarg :width :initform 10000 :export t)
   (height  :accessor height :initarg :height :initform 216 :export t)
   (inverse :accessor inverse :initarg :inverse :initform nil :export t)
   (showgrid :accessor showgrid :initarg :showgrid :initform t :export t)
   (zoom :accessor zoom :initarg :zoom :initform 1.4 :export t)
   (cx :accessor cx :initarg :cx :initform 350 :export t)
   (cy :accessor cy :initarg :cy :initform 360 :export t)
   (w-width :accessor w-width :initarg :w-width :initform 1920 :export t)
   (w-height :accessor w-height :initarg :w-height :initform 1080 :export t)
   (w-x :accessor w-x :initarg :w-x :initform 0 :export t)
   (w-y :accessor w-y :initarg :w-y :initform 0 :export t)
   (gridtype :accessor gridtype :initarg :showgrid :initform "4x4" :export t)
   (elements :accessor elements :initarg :elements :initform nil :export t)
   (last-id :accessor last-id :initarg :last-id :initform 1000 :export t)
   (id-hash :accessor id-hash :initarg :id-hash :initform (make-hash-table) :export t)))

(def-exporting-class svg-item ()
    ((id :accessor id :initarg :id :initform nil :export t
         :documentation "Accessor for the id slot of svg /object/.
@See-also
svg-file
svg-group
svg-layer
svg-line
svg-point
svg-text")
     (svg-class :accessor svg-class :initarg :svg-class :initform nil :export t
                :documentation "Accessor for the svg-class slot of svg /object/.
@See-also
svg-file
svg-group
svg-layer
svg-line
svg-point
svg-text")))

(def-exporting-class svg-group (svg-item)
  ())

(def-exporting-class svg-layer (svg-group)
  ((insensitive :accessor insensitive :initarg :insensitive :initform nil :export t)
   (visible :accessor visible :initarg :visible :initform t :export t)
   (name :accessor name :initarg :name :initform "Ebene" :export t)))

(def-exporting-class svg-tl-layer (svg-group)
  ((insensitive :accessor insensitive :initarg :insensitive :initform nil :export t)
   (visible :accessor visible :initarg :visible :initform t :export t)
   (name :accessor name :initarg :name :initform "Ebene" :export t)))

(def-exporting-class svg-point (svg-item)
  ((cx :accessor cx :initarg :cx :initform 0 :export t)
   (cy :accessor cy :initarg :cy :initform 0 :export t)
   (rx :accessor rx :initarg :rx :initform 10 :export t)
   (ry :accessor ry :initarg :ry :initform 10 :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
   (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd")
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform +black+ :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (stroke-miterlimit :accessor stroke-miterlimit :initarg :stroke-miterlimit :initform 4 :export t)
   (stroke-dasharray :accessor stroke-dasharray :initarg :stroke-dasharray :initform "none" :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (opacity :accessor opacity :initarg :opacity :initform 1 :export t)))

(def-exporting-class svg-line (svg-item)
  ((x1 :accessor x1 :initarg :x1 :initform 0 :export t)
   (y1 :accessor y1 :initarg :y1 :initform 0 :export t)
   (x2 :accessor x2 :initarg :x2 :initform 10 :export t)
   (y2 :accessor y2 :initarg :y2 :initform 0 :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
   (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd" :export t)
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform +black+ :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (marker-end :accessor marker-end :initarg :marker-end :initform nil :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (opacity :accessor opacity :initarg :opacity :initform 1 :export t)
   (attributes :accessor attributes :initarg :attributes :initform "none" :export t)))

(def-exporting-class svg-rect (svg-item)
  ((x :accessor x :initarg :x :initform 0 :export t)
   (y :accessor y :initarg :y :initform 0 :export t)
   (width :accessor width :initarg :width :initform 100 :export t)  
   (height :accessor height :initarg :height :initform 100 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform +black+ :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
   (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd" :export t)
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (opacity :accessor opacity :initarg :opacity :initform 1 :export t)))

(def-exporting-class svg-text (svg-item)
  ((label :accessor label :initarg :label :initform "" :export t)
   (y :accessor y :initarg :y :initform 0 :export t)
   (x :accessor x :initarg :x :initform 0 :export t)
   (attributes :accessor attributes :initarg :attributes :initform nil :export t)
   (text-anchor :accessor text-anchor :initarg :text-anchor :initform "start" :export t)
   (font-size :accessor font-size :initarg :font-size :initform 12 :export t)
   (font-style :accessor font-style :initarg :font-style :initform "normal" :export t)
   (font-weight :accessor font-weight :initarg :font-weight :initform "normal" :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform +black+ :export t)
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform "none" :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (opacity :accessor opacity :initarg :opacity :initform 1 :export t)
   (font-family :accessor font-family :initarg :font-family :initform "Bitstream Vera Sans" :export t)
  ))


(def-exporting-class svg-path (svg-item)
  ((d :accessor d :initarg :d :initform 0 :export t)
   (fill-color :accessor fill-color :initarg :fill-color :initform "none" :export t)
   (fill-rule :accessor fill-rule :initarg :fill-rule :initform "evenodd" :export t)
   (fill-opacity :accessor fill-opacity :initarg :fill-opacity :initform 1 :export t)
   (stroke-color :accessor stroke-color :initarg :stroke-color :initform +black+ :export t)
   (stroke-width :accessor stroke-width :initarg :stroke-width :initform 1 :export t)
   (stroke-linecap :accessor stroke-linecap :initarg :stroke-linecap :initform "butt" :export t)
   (stroke-linejoin :accessor stroke-linejoin :initarg :stroke-linejoin :initform "miter" :export t)
   (marker-end :accessor marker-end :initarg :marker-end :initform nil :export t)
   (stroke-opacity :accessor stroke-opacity :initarg :stroke-opacity :initform 1 :export t)
   (opacity :accessor opacity :initarg :opacity :initform 1 :export t)))



(def-exporting-class svg-clone (svg-item)
  ((href :accessor href :initarg :href :initform "" :export t)
   (transform :accessor transform :initarg :transform :initform "matrix(1,0,0,1,0,0)" :export t)))

;; (make-class-args '(id href transform))



;; helper function to avoid too much typing on class definitions ;-)

(defun make-class-args (liste)
  (format t "~%")
  (loop for arg in liste
     do (format t "~&(~a :accessor ~a :initarg :~a :initform  :export t)~%" arg arg arg)))


;; (make-class-args '("id" "y" "x" "font-size" "font-style" "font-weight" "fill" "fill-opacity" ))

;;;   scratch to be implemented:
;;;
;;;
;;;   <g
;;;      inkscape:label="Ebene 1"
;;;      inkscape:groupmode="layer"
;;;      id="layer1">

;;;     <g
;;;        id="g3179">

;; (defconstant +alphabet+ '(
;; "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
;; ))


(defmacro format-with-slots (stream obj string &rest slots)
  `(let ((ob ,obj))
     (apply #'format 
         (append (list ,stream ,string) 
                 (loop for x in ',slots collect (funcall x ob))))))

(defgeneric print-to-stream (obj stream))
(defmethod print-to-stream ((obj svg-text) stream)
  (with-slots (id y x font-size text-anchor font-style font-weight fill-color fill-opacity stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity opacity font-family label attributes) obj
    (format stream
            "<text
         id=\"text~a\"
         y=\"~a\"
         x=\"~a\"
         style=\"font-size:~apx;font-style:~a;font-weight:~a;opacity:~a;fill:~a;fill-opacity:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;font-family:~a\"~@[
         attributes= \"~a\"~]
         xml:space=\"preserve\"
         text-anchor=\"~a\"><tspan
           y=\"~a\"
           x=\"~a\"
           font-size=\"~apx\"
           id=\"tspan~a\"
           sodipodi:role=\"line\">~a</tspan></text>"
            id y x font-size font-style font-weight opacity (get-color fill-color) fill-opacity (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-opacity font-family attributes text-anchor y x font-size id label)))

;; (print-to-stream (make-instance 'svg-text :label "Testtext" :x 100 :y 117) t)


(defmethod print-to-stream ((obj svg-rect) stream)
    (with-slots (y x height width id fill-color fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin opacity stroke-opacity fill-opacity) obj
      (format stream
              "<rect
         y=\"~a\"
         x=\"~a\"
         height=\"~a\"
         width=\"~a\"
         id=\"rect~a\"
         style=\"fill:~a;fill-rule:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;opacity:~a\" />"
              y x height width id (get-color fill-color) fill-rule (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-opacity opacity)))

;; (print-to-stream (make-instance 'svg-rect :width 117 :height 139 :x 100 :y 117) nil)

(defmethod print-to-stream ((obj svg-line) stream)
  (with-slots
        (fill-color fill-opacity fill-rule opacity stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity
                    x1 y1 x2 y2 id marker-end attributes)
      obj
    (if marker-end
        (format stream
                "    <path
       style=\"fill:~a;fill-opacity:~a;fill-rule:~a;opacity:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a;marker-end:~a\"
       d=\"M ~a,~a L ~a,~a\"
       id=\"path~a\"
       attributes=\"~a\" />
"
                (get-color fill-color) fill-opacity fill-rule opacity (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-opacity marker-end
                x1 y1 x2 y2 id attributes)
        (format stream
                "    <path
       style=\"fill:~a;fill-opacity:~a;fill-rule:~a;opacity:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a\"
       d=\"M ~a,~a L ~a,~a\"
       id=\"path~a\"
       attributes=\"~a\" />
"
                (get-color fill-color) fill-opacity fill-rule opacity (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-opacity
                x1 y1 x2 y2 id attributes))))

;; (print-to-stream (make-instance 'svg-line :x1 :y1 :x2 :y2 :marker-end "url(#Arrow1Lend)") nil)

(defmethod print-to-stream ((obj svg-path) stream)
  (with-slots
        (fill-color fill-opacity fill-rule opacity stroke-color stroke-width stroke-linecap stroke-linejoin stroke-opacity
                    d id marker-end)
      obj
    (format stream
            "   <path
       style=\"fill:~a;fill-opacity:~a;fill-rule:~a;opacity:~a;stroke:~a;stroke-width:~apx;stroke-linecap:~a;stroke-linejoin:~a;stroke-opacity:~a\"
       d=\"~a\"
       id=\"~a\"
       inkscape:connector-curvature=\"0\" />"
            (get-color fill-color) fill-opacity fill-rule opacity (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-opacity
            d (if (numberp id) (format nil "path~d" id) id))))

(defmethod print-to-stream ((obj svg-point) stream)
  (with-slots
        (fill-color fill-opacity fill-rule stroke-color stroke-width stroke-linecap stroke-linejoin stroke-miterlimit stroke-dasharray
                    opacity id cx cy rx svg-class)
      obj
    (format stream
            "    <circle
       r=\"~a\"
       cy=\"~a\"
       cx=\"~a\"
       class=\"~a\"
       style=\"opacity:1;fill:~a;fill-opacity:~a;fill-rule:~a;stroke:~a;stroke-width:~a;stroke-linecap:~a;stroke-linejoin:~a;stroke-miterlimit:~a;stroke-dasharray:~a;stroke-opacity:~a\"
       id=\"~a\"
       />
"
             rx cy cx (or svg-class "undefined") (get-color fill-color) fill-opacity fill-rule (get-color stroke-color) stroke-width stroke-linecap stroke-linejoin stroke-miterlimit stroke-dasharray
             opacity (if (numberp id) (format nil "path~d" id) id))))

(defmethod print-to-stream ((obj svg-clone) stream)
  (with-slots (href id transform) obj
    (format stream
            "    <use
         x=\"0\"
         y=\"0\"
         xlink:href=\"#~a\"
         id=\"use~a\"
         transform=\"~a\"
         height=\"40\"
         width=\"40\" />
"
            href id transform)))



(defgeneric print-head-to-stream (obj stream))
(defgeneric print-tail-to-stream (obj stream))

(defmethod print-head-to-stream ((obj svg-file) stream)
  (let ((background-color (if (sv obj 'inverse) "#000000" "#FFFFFF")))
    (format stream (header obj) background-color (sv obj 'width) (sv obj 'height)
            (sv obj 'width) (sv obj 'height) background-color "0.0"
            (sv obj 'zoom)
            (sv obj 'cx)
            (sv obj 'cy)
            (sv obj 'w-width)
            (sv obj 'w-height)
            (sv obj 'w-x)
            (sv obj 'w-y)
            (if (sv obj 'showgrid) "true" "false")
            (if (string= (sv obj 'gridtype) "4x4") "true" "false")
            (if (string= (sv obj 'gridtype) "5x5") "true" "false"))))

(defmethod print-head-to-stream ((obj svg-layer) stream)
  (with-slots (name id insensitive visible) obj
    (format stream
             "  <g
     inkscape:label=\"~a\"
     inkscape:groupmode=\"layer\"
     id=\"layer~a\"~a>
" name id (with-output-to-string (out)
            (if insensitive
                (format out "~%sodipodi:insensitive=\"true\""))
            (unless visible
              (format out "~%style=\"display:none\""))))))

;; (print-head-to-stream (make-instance 'svg-layer) t)

(defmethod print-head-to-stream ((obj svg-group) stream)
  (with-slots (id) obj
    (format stream
            "<g~%      id=\"~a\">" id)))

;; (print-head-to-stream (make-instance 'svg-group) t)

(defmethod print-tail-to-stream ((obj svg-layer) stream)
  (format stream "  </g>~%"))

(defmethod print-head-to-stream ((obj svg-tl-layer) stream)
  (with-slots (name id insensitive visible) obj
    (format stream
             "  <g
     inkscape:label=\"~a\"
     inkscape:groupmode=\"layer\"
     id=\"layer-~a\"~a
     transform=\"translate(0, 150) scale(1, -1)\" >
" name id (with-output-to-string (out)
            (if insensitive
                (format out "~%sodipodi:insensitive=\"true\""))
            (unless visible
              (format out "~%style=\"display:none\""))))))

(defmethod print-tail-to-stream ((obj svg-tl-layer) stream)
  (format stream "  </g>~%"))

(defmethod print-tail-to-stream ((obj svg-group) stream)
  (format stream "  </g>~%"))

(defmethod print-tail-to-stream ((obj svg-file) stream)
  (format stream "  </svg>~%"))

(defun new-id (svg-file id-type)
  (incf (gethash id-type (slot-value svg-file 'id-hash) 0)))

(defun print-elements-to-stream (elems stream)
  (cond ((null elems) nil)
        ((consp (car elems))
         (progn
           (print-head-to-stream (caar elems) stream)
           (print-elements-to-stream (cdar elems) stream)
           (print-tail-to-stream (caar elems) stream)
           (print-elements-to-stream (cdr elems) stream)))
        (t (progn
           (print-to-stream (car elems) stream)
           (print-elements-to-stream (cdr elems) stream)))))
