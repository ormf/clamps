;;; svg-import.lisp
;;;
;;; functions for importing point or line elements from an svg
;;; file. Transformations are tracked through the svg tree and applied
;;; accordingly to obtain the correct absolute coordinates of the
;;; elements.
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(in-package :svg-import-export)

;;; Matrix transformations:

(defun mtx-mult (m1 m2)
    "multiply two 2x3 matrixes given as lists of the format
     (x11 y11 x12 y12 x13 y13) as in svg transforms."
  (cond ((and m1 m2)
         (destructuring-bind (x11 y11 x12 y12 x13 y13) m1
           (destructuring-bind (x21 y21 x22 y22 x23 y23) m2
             (list
              (+ (* x11 x21) (* x12 y21))   ;;; x31
              (+ (* y11 x21) (* y12 y21))   ;;; y31
              (+ (* x11 x22) (* x12 y22))   ;;; x32
              (+ (* y11 x22) (* y12 y22))   ;;; y32
              (+ (* x11 x23) (* x12 y23) x13) ;;; x33
              (+ (* y11 x23) (* y12 y23) y13) ;;; y33
              ))))
        (m1 m1)
        (m2 m2)
        (t nil)))

;;; svg transformation functions returning the transformation matrix
;;; of their operation

(defun matrix (x1 y1 x2 y2 x3 y3)
  (list x1 y1 x2 y2 x3 y3))

(defun translate (tx &optional (ty 0))
  (list 1 0 0 1 tx ty))

(defun ang->rad (angle)
  (* (mod angle 360) 2/360 pi))

(defun rotate (angle)
  (let ((rad (ang->rad angle)))
    (list (cos rad) (sin rad) (- (sin rad)) (cos rad) 0 0)))

(defun scale (sx sy)
  (list sx 0 0 sy 0 0))

(defun x-skew (angle)
  (list 1 0 (tan angle) 1 0 0))

(defun y-skew (angle)
  (list 1 (tan angle) 0 1 0 0))

(defun skew (x-angle y-angle)
  (list 1 (tan y-angle) (tan x-angle) 1 0 0))
        
;;; parsing of svg transformation string:

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
       while j))

(defun read-op (str)
  "parse a svg transformation string and return the transformation
function and its arguments as a list. Returns nil for an empty
transformation string."
  (unless (string= str "")
    (let ((split-point (position #\( str)))
      (list
       (symbol-function
        (intern (string-upcase (subseq str 0 split-point)) :svg-ie))
       (read-from-string (substitute #\SPACE #\, (subseq str split-point)))))))

;;; (read-op "translate(12,27)") -> (translate (12 27))
;;; (read-op "") -> nil

(defun get-transformation-mtx (transforms)
  "returns the 2x3 matrix of the supplied svg transformation string
   (a series of transformations separated by a single space). The
transformations are executed from left to right."
  (reduce
   #'mtx-mult
   (reverse
    (loop for transformation in
         (split-by-one-space transforms)
       collect (destructuring-bind (fn args)
                   (read-op transformation)
                 (apply fn args))))))


#|
(get-transformation-mtx
 "translate(196.02814,156.95225) matrix(0.9330498,0,0,2.8503305,0.24844852,-155.11236)")
-> (0.9330498 0.0 0.0 2.8503306 183.15245 292.25345)
|#

(defun vec-mtx-mult (vec mtx)
  "multiply a 2-dimensional vector with a 2x3 matrix"
  (if mtx
      (destructuring-bind (x1 y1 x2 y2 x3 y3) mtx
        (destructuring-bind (x y) vec
          (list
           (+ (* x x1) (* y x2) x3)
           (+ (* x y1) (* y y2) y3))))
      vec))

#|
(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "translate(2,4) rotate(90)"))

(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "rotate(90) translate(2,4)"))

(vec-mtx-mult
 '(0 0)
 (get-transformation-mtx
  "rotate(3.14159)"))

(get-transformation-mtx
 "translate(196.02814,156.95225) matrix(0.9330498,0,0,2.8503305,0.24844852,-155.11236)")


(get-transformation-mtx
 "translate(2,4) rotate(3.14159)")


(get-transformation-mtx
 "rotate(3.14159) translate(2,4)")
|#

;;;
;;; xml parsing:
;;;

(defun get-layer (layer-local-name xml &key (URI "http://www.inkscape.org/namespaces/inkscape"))
  (cxml-stp:find-recursively-if
   (lambda (child)
     (and (typep child 'cxml-stp:element)
	  (equal (cxml-stp:local-name child) "g")
          (if (cxml-stp:find-attribute-named child "groupmode" URI)
              (equal (cxml-stp:value (cxml-stp:find-attribute-named child "groupmode" URI)) "layer"))
          (if
           (cxml-stp:find-attribute-named child "label" URI)
           (equal (cxml-stp:value (cxml-stp:find-attribute-named child "label" URI)) layer-local-name))))
   xml))

(defun group? (node)
  (and (typep node 'cxml-stp:element)
       (equal (cxml-stp:local-name node) "g")))

(defun layer? (node &key (URI "http://www.inkscape.org/namespaces/inkscape"))
     (and (typep node 'cxml-stp:element)
	  (equal (cxml-stp:local-name node) "g")
          (if (cxml-stp:find-attribute-named node "groupmode" URI)
              (equal (cxml-stp:value (cxml-stp:find-attribute-named node "groupmode" URI)) "layer"))
          (cxml-stp:find-attribute-named node "label" URI)))

(defun layer-name (node &key (URI "http://www.inkscape.org/namespaces/inkscape"))
  (cxml-stp:value (cxml-stp:find-attribute-named node "label" URI)))


#|
(defun visible? (node &key (URI "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"))
  (if (and (cxml-stp:find-attribute-named node "label" URI)
           (cxml-stp:find-attribute-named node "style"))
      (break "layer: ~a, type: ~a val" (cxml-stp:value (cxml-stp:find-attribute-named node "label" URI))
             (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
  (and (typep node 'cxml-stp:element)
       (equal (cxml-stp:local-name node) "g")
       (if (and (cxml-stp:find-attribute-named node "groupmode" URI)
                (equal (cxml-stp:value (cxml-stp:find-attribute-named node "groupmode" URI)) "layer"))
           (let ((val (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
             (and val (not (string= val "display:none"))))
           t)))
|#

(defun visible? (node &key (URI "http://www.inkscape.org/namespaces/inkscape"))
  (if (and (typep node 'cxml-stp:element)
           (equal (cxml-stp:local-name node) "g")
           (if (cxml-stp:find-attribute-named node "groupmode" URI)
               (equal (cxml-stp:value (cxml-stp:find-attribute-named node "groupmode" URI)) "layer"))
           (cxml-stp:find-attribute-named node "style"))
      (not (cl-ppcre:scan "display:none" (cxml-stp:value (cxml-stp:find-attribute-named node "style"))))
      t))



#|
(defun point? (node &key (URI "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"))
  (and (typep node 'cxml-stp:element)
       (string= (cxml-stp:value (cxml-stp:find-attribute-named node "type" URI)) "arc")))
|#

(defun circle? (node)
  (and (typep node 'cxml-stp:element)
       (or (string= (cxml-stp:local-name node) "circle")
           (string= (cxml-stp:local-name node) "ellipse"))))

(defun path? (node)
  (and (typep node 'cxml-stp:element)
       (string= (cxml-stp:local-name node) "path")))

#|
;;; without quantisation

(defun get-coords (node transformation)
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list x (* y -1))))

;;; Quantisierung auf 1/4-Töne

(defun get-coords (node transformation)
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list x (* (round (* 2 y)) -0.5))))

;;; Quantisierung auf 1/4-Töne und ganze Schläge
|#

(defun get-coords (node transformation &key (xquantize t) (yquantize t))
  (destructuring-bind (x y)
      (apply #'vec-mtx-mult 
             (list
              (list
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cx")))
               (read-from-string (cxml-stp:value (cxml-stp:find-attribute-named node "cy"))))
              transformation))
    (list
     (if xquantize
         (* (round (* 2 x)) 0.5)
         x)
     (if yquantize
         (* (round (* 2 y)) 0.5)
         y))))

(defun style-stroke-width (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "stroke-width:\([0-9\.]\+\)px" style-string)
    (declare (ignore _1 _2))
    (if rstart
        (read-from-string (subseq style-string (aref rstart 0) (aref rend 0))))))

;;; (style-stroke-width "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")

(defun style-stroke-color (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "stroke:\(#[0-9abcdefABCDEF]\+\)" style-string)
    (declare (ignore _1 _2))
    (if rstart
        (subseq style-string (aref rstart 0) (aref rend 0)))))

;;; (style-stroke-color "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1")

(defun style-stroke-opacity (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "(?:^|;)stroke-opacity:\([0-9\.]\+\)" style-string)
    (declare (ignore _1 _2))
    (if rstart (read-from-string (subseq style-string (aref rstart 0) (aref rend 0)))
        (break "style-stroke-opacity not readable: ~s" style-string))))

(defun style-opacity (style-string)
  (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "(?:^|;)opacity:\([0-9\.]\+\)" style-string)
    (declare (ignore _1 _2))
    (if rstart (read-from-string (subseq style-string (aref rstart 0) (aref rend 0)))
        (style-stroke-opacity style-string))))

;;; (style-stroke-opacity "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.5px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:0.35")

(defun parse-color-opacity (str)
  (list
   (style-stroke-color str)
   (style-stroke-opacity str)))

(defun parse-path (str)
  (let ((res (read-from-string (format nil "(~a)" (substitute #\SPACE #\, (subseq str 2 nil))))))
    (if (= 4 (length res))
        (destructuring-bind (x y x2 y2) res
            (list (list x y) (list x2 y2)))
        (destructuring-bind (x y op x2 dy) res
          (declare (ignore op))
          (list (list x y) (list x2 (+ y dy)))))))

(defun make-string-parser (str)
  "return a closure, which returns all space-delimited substrings of
str in order, one on each call of the closure. Returns nil if string
is exhausted."
  (let ((i 0) (j 0))
    (lambda ()
      (if j
          (progn
            (setf j (position #\Space (string-left-trim '(#\Space #\Tab #\Newline) str) :start i))
            (prog1
                (subseq str i j)
              (if j (setf i (1+ j)))))))))

#|
(defparameter fn (make-string-parser (substitute #\SPACE #\, "m 10,40 l 5,8")))
(funcall fn)
"m 10,40 l 5,8"
(parse-path2 "M 10,40 15,8")
|#

(defun move-abs (curr-coords mv-coords)
  (declare (ignore curr-coords))
  mv-coords)

(defun lineto-abs (curr-coords mv-coords)
  (declare (ignore curr-coords))
  mv-coords)

(defun hlineto-abs (curr-coords mv-coords)
  (list (first mv-coords) (second curr-coords)))

(defun move-rel (curr-coords mv-coords)
  (mapcar #'+ curr-coords mv-coords))

(defun lineto-rel (curr-coords mv-coords)
  (mapcar #'+ curr-coords mv-coords))

(defun vlineto-abs (curr-coords mv-coords)
  (list (first curr-coords) (second mv-coords)))

(defun vlineto-rel (curr-coords mv-coords)
  (list (first curr-coords) (+ (second curr-coords) (first mv-coords))))

(defun parse-path2 (str)
  (if (string/= str "")
      (let ((fn (make-string-parser (substitute #\SPACE #\, str)))
            (cmd nil)
            (curr-coords '(0 0)))
        (mapcar
         (lambda (x) (setf curr-coords (funcall (first x) curr-coords (rest x))))
         (loop
            for s = (funcall fn)
            while s
            collect (cond
                      ((string= s "m")
                       (progn
                         (setf cmd 'lineto-rel)
                         (list 'move-rel (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                      ((string= s "l")
                       (progn
                         (setf cmd 'lineto-rel)
                         (list 'lineto-rel (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                      ((string= s "h")
                       (progn
                         (setf cmd 'lineto-rel)
                         (list 'lineto-rel (read-from-string (funcall fn)) 0)))
                      ((string= s "v")
                       (progn
                         (setf cmd 'vlineto-rel)
                         (list 'vlineto-rel (read-from-string (funcall fn)) (first curr-coords))))
                      ((string= s "M")
                       (progn
                         (setf cmd 'lineto-abs)
                         (list 'move-abs  (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                      ((string= s "L")
                       (progn
                         (setf cmd 'lineto-abs)
                         (list 'lineto-abs  (read-from-string (funcall fn)) (read-from-string (funcall fn)))))
                      ((string= s "H")
                       (progn
                         (setf cmd 'hlineto-abs)
                         (list 'hlineto-abs (read-from-string (funcall fn)) (first curr-coords))))
                      ((string= s "V")
                       (progn
                         (setf cmd 'vlineto-abs)
                         (list 'vlineto-abs (read-from-string (funcall fn)) (first curr-coords))))
                      (t (list cmd (read-from-string s) (read-from-string (funcall fn))))))))))

#|

(parse-path2 "m 791.74452,-73.99904 16,0")
(parse-path2 "m 791.74452,-73.99904 h 16")
(parse-path2 "m 791.74452,-73.99904 v 0")
(parse-path2 "m 791.74452,-73.99904 H 800")
(subseq "1 2 3" 3 nil)

(parse-path2 "M 10,40 5,8")
|#

(defun get-path-coords (node parse-state &key (x-offset 0) (timescale 1) (xquantize t) (yquantize t))
  "return (list x y length color opacity) from path."
  (let* ((path (parse-path2 (cxml-stp:value (cxml-stp:find-attribute-named node "d"))))
         (style-string (cxml-stp:value (cxml-stp:find-attribute-named node "style")))
         (transformation (update-transformation (svg-parse-state-transformation parse-state) node)))
    (if path
        (destructuring-bind ((x1 y1) (x2 y2))
            (sort
             (list (funcall #'vec-mtx-mult (first path) transformation)
                   (funcall #'vec-mtx-mult (second path) transformation))
             #'< :key #'first)
          (declare (ignore y2))
          (list
           (if xquantize
               (* (round (* 2 timescale (+ x-offset x1))) 0.5)
               (+ x-offset x1))
           (if yquantize
               (round (* 1 y1))
               (* 1 y1))
           (if xquantize
               (round (* timescale (- x2 x1)))
               (- x2 x1))
           (style-stroke-color style-string)
           (* (svg-parse-state-opacity parse-state)
              (style-opacity style-string))))
        (warn "~a is empty!" (cxml-stp:value (cxml-stp:find-attribute-named node "id"))))))



(defun update-transformation (curr-transformation node)
  (let ((new-transform (cxml-stp:find-attribute-named node "transform")))
    (if new-transform
        (mtx-mult
         curr-transformation
         (get-transformation-mtx
          (cxml-stp:value new-transform)))
        curr-transformation)))

;;; "opacity:0.22"

(declaim (inline style-string))
(defun style-string (node)
  (alexandria:if-let ((style-attr (cxml-stp:find-attribute-named node "style")))
    (cxml-stp:value style-attr)))

(defun get-opacity (node)
  (let* ((style-string (style-string node)))
    (and style-string
         (multiple-value-bind (_1 _2 rstart rend) (cl-ppcre:scan "opacity:\([0-9\.]\+\)" style-string)
    (declare (ignore _1 _2))
    (if rstart
        (read-from-string (subseq style-string (aref rstart 0) (aref rend 0))))))))

(defun update-state (state node)
  (let ((new-transform (cxml-stp:find-attribute-named node "transform"))
        (opacity (get-opacity node)))
    (if new-transform
        (setf (svg-parse-state-transformation state)
              (mtx-mult
               (svg-parse-state-transformation state)
               (get-transformation-mtx
                (cxml-stp:value new-transform))))
        state)
    (if opacity
        (setf (svg-parse-state-opacity state)
              (* opacity (svg-parse-state-opacity state))))
    state))

(defun get-fill-opacity (node)
  (read-from-string
   (aref
    (second (multiple-value-list
             (cl-ppcre:scan-to-strings
              "fill-opacity:([0-9.]+)"
              (cxml-stp:value (cxml-stp:find-attribute-named node "style")))))
    0)))

(defun get-fill-color (node)
  (aref
   (or (second (multiple-value-list
                (cl-ppcre:scan-to-strings
                 "fill:(#[0-9a-fA-F]+)"
                 (cxml-stp:value (cxml-stp:find-attribute-named node "style")))))
       #("#000000"))
   0))


(defun get-id (node)
  (cxml-stp:value (cxml-stp:find-attribute-named node "id")))

(defun collect-points (layer transformation &key (xquantize t) (yquantize t))
  (let ((result '()))
    (cxml-stp:map-children
     'list
     (lambda (child)
       (cond
         ((and (group? child) (visible? child))
          (let ((inner-transformation (update-transformation transformation child)))
            (let ((res (collect-points child inner-transformation :xquantize xquantize
                                       :yquantize yquantize)))
              (if res (push res result)))))
         ((circle? child) (push (append
                                 (get-coords child transformation :xquantize xquantize
                                             :yquantize yquantize)
                                 (list (get-fill-color child)
                                       (get-fill-opacity child)))
                                result))))
     layer)
    (reverse result)))

(defun svg-ie-get-path-coords (node parse-state &key (x-offset 0) (timescale 1) (xquantize nil) (yquantize nil))
  "return a list from path node."
  (let* ((path (parse-path2 (cxml-stp:value (cxml-stp:find-attribute-named node "d"))))
         (style-string (cxml-stp:value (cxml-stp:find-attribute-named node "style")))
         (attributes (and (cxml-stp:find-attribute-named node "attributes")
                          (cxml-stp:value (cxml-stp:find-attribute-named node "attributes"))))
         (transformation (update-transformation (svg-parse-state-transformation parse-state) node)))
    (if path
        (destructuring-bind ((x1 y1) (x2 y2))
            (sort
             (list (funcall #'vec-mtx-mult (first path) transformation)
                   (funcall #'vec-mtx-mult (second path) transformation))
             #'< :key #'first)
          (list
           :x1
           (if xquantize
               (* (round (* 2 timescale (+ x-offset x1))) 0.5)
               (+ x-offset (* timescale x1)))
           :y1
           (if yquantize
               (round (* 1 y1))
               (* 1 y1))
           :x2
           (if xquantize
                  (round (* timescale x2))
                  (+ x-offset (* timescale x2)))
           :y2
           (if yquantize
               (round (* 1 y2))
               (* 1 y2))
           :color
           (style-stroke-color style-string)
           :opacity
           (* (svg-parse-state-opacity parse-state)
              (style-opacity style-string))
           :attributes (if (and attributes (string/= (string-upcase attributes) "NONE"))
                           (read-from-string (format nil "(~a)" (quote-svg-attr-props attributes))))
           ))
        (warn "~a is empty!" (cxml-stp:value (cxml-stp:find-attribute-named node "id"))))))

(defun svg-collect-lines (layer parse-state &key (timescale 1) (x-offset 0) (xquantize nil) (yquantize nil) layer?)
  "return a property list of all lines in layer with a given parse-state."
  (let ((result '()))
    (if (and layer (visible? layer))
        (progn
          (cxml-stp:map-children
           'list
           (lambda (child)
             (cond
               ((and layer? (layer? child) (visible? child))
                (let ((inner-parse-state (update-state (copy-svg-parse-state parse-state) child)))
                  (let ((name (layer-name child))
                        (res (svg-collect-lines child inner-parse-state
                                            :x-offset x-offset
                                            :timescale timescale
                                            :xquantize xquantize
                                            :yquantize yquantize
                                            :layer? layer?)))
                    (if res (setf result (append (list (list :layer name :contents res)) result))))))
               ((and (group? child) (visible? child))
                (let ((inner-parse-state (update-state (copy-svg-parse-state parse-state) child)))
                  (let ((res (svg-collect-lines child inner-parse-state
                                            :x-offset x-offset
                                            :timescale timescale
                                            :xquantize xquantize
                                            :yquantize yquantize
                                            :layer? layer?)))
                    (if res (push res result)))))
               ((path? child) (ou:push-if (svg-ie-get-path-coords child parse-state
                                                                  :xquantize xquantize
                                                                  :yquantize yquantize
                                                                  :x-offset x-offset
                                                                  :timescale timescale)
                                          result))))
           layer)
          (reverse result)))))

(defun find-element (local-name xml)
  (cxml-stp:find-recursively-if
   (lambda (child)
     (and (typep child 'cxml-stp:element)
	  (equal (cxml-stp:local-name child) local-name)))
   xml))

;; (make-pathname :directory '(:absolute "tmp") :name "test.svg")

(defun get-points-from-file (&key (fname #P"/tmp/test.svg") (xquantize t)(x-offset 0) (timescale 1) (yquantize t) (layer-name "Punkte"))
  "extract all circle objects (points) in the layer \"Punkte\" of svg infile."
  (declare (ignore timescale x-offset))
  (collect-points
   (get-layer
    layer-name
    (cxml:parse-file
     fname
     (stp:make-builder)))
   nil :xquantize xquantize :yquantize yquantize))

(defstruct svg-parse-state
  (transformation nil)
  (opacity 1.0))

(defun sanitize-filename (fname)
  (let ((filename (namestring fname)))
    (if (char= (aref filename 0) #\~)
        (merge-pathnames (string-left-trim '(#\/) (subseq filename 1))
                         (user-homedir-pathname))
        (pathname fname))))

(defun svg-get-lines-from-file (&key (fname #P"/tmp/test.svg") (x-offset 0) (timescale 1) (xquantize nil) (yquantize nil) (layer-name "Events") layer?)
  "extract all lines in the layer <layer-name> of svg infile."
  (svg-collect-lines
   (get-layer
    layer-name
    (cxml:parse-file
     (sanitize-filename fname)
     (stp:make-builder)))
   (make-svg-parse-state)
   :xquantize xquantize :yquantize yquantize
   :x-offset x-offset :timescale timescale
   :layer? layer?))

#|

(loop
   for x in (get-points-from-file)
   collect (sort x #'(lambda (x y) (< (first x) (first y)))))

   (get-layer
    "Punkte"
    (cxml:parse
     #P"/tmp/testchords02.svg"
     (stp:make-builder)))

|#

#|
(get-layer

"Linien"
 (cxml:parse
  #P"/tmp/test.svg"
  (stp:make-builder)))
|#
