;; vbap.lisp
;;
;; VBAP originally created by Ville Pukki
;; This version is a complete reimplementation
;; of the ver 0.99 PD code by Ville Pukki
;; and Scott Wilson's supercollider adoption according to the
;; paper "Creating Auditory Displays with Multiple Loudspeakers Using
;; VBAP: A Case Study with DIVA Project" by Ville Pukki.
;;
;; The original C-code was written by Ville Pulkki 1999
;; Helsinki University of Technology
;; and
;; University of California at Berkeley
;;
;; Copyright (C) 2016  Orm Finnendahl
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(in-package :cl-user)

(defmacro map-indexed (result-type fn &rest seqs)
  "map fn over seqs with incrementing idx. The idx will get supplied
as first arg to fn and is reset for each seq."
  (let ((i (gensym)))
    `(let ((,i -1))
       (map ,result-type
        (lambda (&rest args)
          (apply ,fn (incf ,i) args))
        ,@seqs))))

(defun dround (num &optional (prec 2))
  "round num to prec digits after the decimal point."
  (let ((ept (expt 10 prec)))
    (/ (round (* num ept)) ept 1.0)))

(defun filter (pred seq)
  "return a list of all elements of seq satisfying pred."
  (remove-if-not pred seq))

(defun flatten (list)
  "remove all brackets except the outmost in list."
  (loop for i in list
     append (if (consp i) (flatten i) (list i))))

;;; vector utility functions: Vectors here are expected and
;;; implemented as lists, not as common lisp vectors!

(defun sqr (n) (* n n))

;; Caution: The following functions when having two or more vectors as
;; arguments don't check that all vectors (lists) have equal
;; length. They will only return their result based on all vectors
;; being truncated to the shortest vector given.

(defun v+ (&rest args) (apply #'mapcar #'+ args))
(defun v- (&rest args) (apply #'mapcar #'- args))
(defun v= (&rest args) (not (member nil (apply #'mapcar #'eql args))))
(defun v-len-sqr (v) (reduce #'+ (mapcar #'sqr v)))
(defun v-len (v) (sqrt (v-len-sqr v)))

(defun v* (u v)
  (cond
    ((and (consp u) (consp v)) (mapcar #'* u v))
    ((consp u) (mapcar (lambda (coord) (* v coord)) u))
    ((consp v) (mapcar (lambda (coord) (* u coord)) v))))

(defun v-neg (u) (v* -1 u))
(defun v-dist (u v) (v-len (v- u v)))
(defun v-dist-sqr (u v) (v-len-sqr (v- u v)))

(defun v-setlen (u len)
  (let ((dim (length u)))
    (if (every #'zerop u) ; [0 0 ...]?
      (cons len (loop for c from 1 below dim collect 0))         ; [t 0 ...]
      (v* u (/ len (v-len u))))))

(defun v-norm (u)
  (v-setlen u 1))

(defun v-dot (u v)
  (reduce #'+ (v* u v)))

(defun v-cross-prod (v1 v2)
  (destructuring-bind (x1 y1 z1) v1
    (destructuring-bind (x2 y2 z2) v2
      (v- (v* (list y1 z1 x1) (list z2 x2 y2))
          (v* (list z1 x1 y1) (list y2 z2 x2))))))

(defun v-unq-cross-prod (u v)
  (v-norm (v-cross-prod u v)))

(defun norm-clip (n)
  "clip n to range [-1..1]. Returns a float."
  (float (* (signum n) (min 1 (abs n)))))

(defun v-angle (u v)
  "return angle between u and v (any dimension) in radians."
  (abs
   (acos
    (norm-clip
     (/ (v-dot u v)
        (* (v-len u) (v-len v)))))))

(defun mtx-transpose (m)
  (apply #'mapcar #'list m))

(defun inv-det (v1 v2 &optional v3)
  "calc inverse-determinant of 2x2 or 3x3 matrices supplied as seq of
  2-d/3-d vectors."
  (if v3
      (/ (reduce #'+ (v* v1 (v-cross-prod v2 v3))))
      (destructuring-bind (x1 y1) v1
        (destructuring-bind (x2 y2) v2
          (/ (- (* x1 y2) (* x2 y1)))))))

(defun mtx-2d-inverse (m)
  (destructuring-bind (v1 v2) m
    (destructuring-bind ((x1 y1) (x2 y2)) m
      (mapcar (lambda (m) (v* (inv-det v1 v2) m))
              (list (list y2 (- y1))
                    (list (- x2) x1))))))

(defun mtx-3d-inverse (m)
;;; caution: Not in original vbap code!
;;; (mtx-transpose
  (destructuring-bind (v1 v2 v3) m
    (mapcar (lambda (v4 v5)
              (v* (inv-det v1 v2 v3)
                  (v-cross-prod v4 v5)))
            (list v2 v3 v1)
            (list v3 v1 v2)))
  ;;; )
  )

(defun coords (ls) (getf ls :coords))
(defun azi (ls) (getf ls :azi))
(defun ele (ls) (getf ls :ele))
(defun chan-offset (ls) (getf ls :chan-offset))

(defun get-coords(speaker-set)
  (mapcar #'coords speaker-set))

(defun get-ls-nos (speaker-set)
  (mapcar (lambda (ls) (1+ (chan-offset ls))) speaker-set))

(defun get-offsets (speaker-set)
  (mapcar #'chan-offset speaker-set))

(defun idx->coords (idx speakers)
  "return the coords of a speaker with given idx (= chan-offset)."
  (loop
     for ls in speakers
     if (= idx (chan-offset ls)) return (coords ls)))

(defun all-combinations (l n)
  "return a list with all n combinations of a seq l."
  (cond ((= n 1) (mapcar #'list l))
        ((null l) ())
        (t (append
            (mapcar (lambda (x) (cons (first l) x))
                 (all-combinations (rest l) (1- n)))
            (all-combinations (rest l) n)))))

(defun ang->cart (azi &optional ele)
  "multi-arity (2-d and 3-d) function returning a vector with
  cartesian coordinates of a given azimuth angle (or azimuth
  angle/elevation) pair."
  (let ((atorad (* PI 2/360)))
    (if ele
        (list
         (* (cos (* azi atorad)) (cos (* ele atorad)))
         (* (sin (* azi atorad)) (cos (* ele atorad)))
         (sin (* ele atorad)))
        (list
         (* (cos (* azi atorad)))
         (* (sin (* azi atorad)))))))

(defun fit-angle (angle)
  "fit angle into the interval ]-180..180] degrees"
  (let ((modangle (mod (+ angle 180) 360)))
    (if (= modangle 0) 180 (- modangle 180))))

(defun init-speaker (idx dir)
  "given an index (= chan-offset) and a direction (azimuth number for
  2-d and azimuth/elevation list for 3-d) return a property list with
  all necessary information for a single speaker."
  (if (numberp dir) ;;; 2-d or 3-d?
      (let ((fitted-azi (fit-angle dir)))
        (list :azi fitted-azi
              :coords (mapcar (lambda (x) (dround x 3))
                              (ang->cart fitted-azi))
              :chan-offset idx))
      (destructuring-bind (azi ele) dir
        (let ((fitted-azi (fit-angle azi)))
          (list :azi fitted-azi
                :ele ele
                :coords (mapcar (lambda (x) (dround x 3))
                                (ang->cart fitted-azi ele))
                :chan-offset idx)))))

(defun get-speaker-maps (directions)
  "calculate the property lists for all speakers defined by directions."
  (map-indexed 'list #'init-speaker directions))

;;; 3-d helper functions

(defun ls-outside? (ls inv-matrix)
  "given the coordinate vector of a speaker and the inverse matrix of
  a speaker triplet, check, if the speaker is outside the area of the
  triplet."
  (some (lambda (v) (< (v-dot ls v) -0.001)) inv-matrix))

(defun ls-equal-p (l1 l2)
  "determine equality of two speakers by comparing their chan-offsets."
  (= (chan-offset l1) (chan-offset l2)))

(defun remove-triplet (triplet speakers)
  "remove all speakers of triplet from speakers sequence."
  (labels ((remove-ls-seq (ls-seq speakers)
             (if (null ls-seq) speakers
                 (remove-ls-seq (cdr ls-seq)
                                (remove (first ls-seq) speakers
                                        :test #'ls-equal-p)))))
    (remove-ls-seq triplet speakers)))

(defun every-ls-outside-triplet? (triplet speakers)
  "check whether every speaker in the speakers seq apart from the
  triplet speakers themselves is outside the area of the triplet."
  (let ((inv-mtx (mtx-3d-inverse (get-coords triplet))))
    (every (lambda (ls) (ls-outside? (coords ls) inv-mtx))
            (remove-triplet triplet speakers))))

(defun vol-p-side-lgth (triplet)
  "calculate volume of the parallelepiped defined by the loudspeaker
  direction vectors and divide it with the total length of the
  triangle sides. This is used when removing too narrow triangles."
  (destructuring-bind  (v1 v2 v3) triplet
    (let ((volper (abs (v-dot (v-unq-cross-prod v1 v2) v3)))
          (lgth (reduce #'+ (mapcar (lambda (pair) (apply #'v-angle pair))
                                    `((,v1 ,v2) (,v1 ,v3) (,v2 ,v3))))))
      (if (> lgth 0.00001) (/ volper lgth) 0))))

(defun lines-intersect? (&rest pairs)
  "check if lines i j and k l intersect on the unit sphere."
  (destructuring-bind ((i j) (k l)) pairs
    (let* ((v3 (v-unq-cross-prod ;;; intersection point of planes ij and kl on unit sphere
                (v-unq-cross-prod i j)
                (v-unq-cross-prod k l)))
           (nv3 (v-neg v3)) ;;; intersection point on opposite side of unit sphere
           (d-ij (v-angle i j)) (d-kl (v-angle k l)) ;;; distances between points
           (d-iv3 (v-angle i v3)) (d-jv3 (v-angle j v3))
           (d-kv3 (v-angle k v3)) (d-lv3 (v-angle l v3))
           (d-inv3 (v-angle i nv3)) (d-jnv3 (v-angle j nv3))
           (d-knv3 (v-angle k nv3)) (d-lnv3 (v-angle l nv3)))
      (and
;;; no speaker close to crossing points
       (every (lambda (d) (> (abs d) 0.01))
              (list d-iv3 d-jv3 d-kv3 d-lv3 d-inv3 d-jnv3 d-knv3 d-lnv3))
;;; crossing point is on lines between both speaker pairs
       (or (and (<= (abs (- d-ij (+ d-iv3 d-jv3))) 0.01)
                (<= (abs (- d-kl (+ d-kv3 d-lv3))) 0.01))
           (and (<= (abs (- d-ij (+ d-inv3 d-jnv3))) 0.01)
                (<= (abs (- d-kl (+ d-knv3 d-lnv3))) 0.01)))))))

(defun ls-pair-equal-p (p1 p2)
  (= (length (remove-duplicates (append p1 p2) :test #'ls-equal-p)) 2))

(defun speakers (plist) (getf plist :speakers))

(defun dist (plist) (getf plist :dist))

(defun get-all-connections (available-triplets)
  "return all possible speaker connections as lists, sorted by their
distance."
  (mapcar #'speakers
          (sort (mapcar (lambda (p) (list :speakers p
                                     :dist (apply #'v-angle (mapcar #'coords p))))
;;; reduce the sets of all possible pairs in all available triplets
;;; and remove any duplicates regardless of the ordering of the pairs
;;; themselves:
                        (remove-duplicates
                         (apply #'append (mapcar
                                          (lambda (triplet) (all-combinations triplet 2))
                                          available-triplets))
                         :test #'ls-pair-equal-p)) 
;;; collect maps of all possible speaker pairs and their distances
                (lambda (p1 p2) (< (dist p1) (dist p2)))))) ;; sort pairs by distance

;;; while traversing the connections list (provided as a seq of
;;; speaker pairs sorted by distance (see get-all-connections)),
;;; remove all subsequent pairs which intersect the current pair. The
;;; returned result will contain no intersecting connections.

(defun remove-intersecting-pairs (connections)
  (cond ((null connections) '())
        (t (cons (first connections)
                 (remove-intersecting-pairs
                  (filter (lambda (pair)
                            (not (lines-intersect?
                                  (mapcar #'coords (first connections))
                                  (mapcar #'coords pair))))
                          (rest connections)))))))

(defun contained? (pair connections)
  "check whether ls-pair is contained in all connections."
  (member pair connections :test #'ls-pair-equal-p))

(defun triplet-connectable? (triplet connections)
  "is every speaker pair in the triplet contained in connections?"
  (every (lambda (pair) (contained? pair connections))
          (all-combinations triplet 2)))

;;; main function for 3d: Return all triplets, which cover the whole
;;; 3-d space of the speaker-arrangement. The speaker triplets are
;;; returned as lists of speaker maps, each map containing the
;;; azimuth, elevation, coordinates and channel-offset of an
;;; individual speaker.

(defun get-3d-triplets (speakers)
  (let* ((all-valid-triplets (filter (lambda (triplet)
                                       (> (vol-p-side-lgth (get-coords triplet))
                                          0.01))
                                     (all-combinations speakers 3)))
         (all-connections (get-all-connections all-valid-triplets))
         (valid-connections (remove-intersecting-pairs all-connections)))
    (filter (lambda (triplet)
              (and (triplet-connectable? triplet valid-connections)
                   (every-ls-outside-triplet? triplet speakers)))
            all-valid-triplets)))

;;; collect the speaker numbers, the inverse-matrix and matrix for one
;;; single speaker triplet into a list.

(defun collect-3d-vbap-data (triplet)
  (let ((coords (get-coords triplet))
        (ls-nos (get-ls-nos triplet)))
    (flatten (append ls-nos
                     (mtx-3d-inverse coords)
                     (mtx-transpose coords)))))

;;; collect the data of all triplets into a single one-dimensional
;;; vector containing the data of all 3d-triplets suitable for the
;;; vbap ugen.

(defun get-3d-vbap-buffer-data (speakers)
  (apply #'append
         (list 3.0 (float (length speakers)))
         (mapcar (lambda (triplet) (mapcar (lambda (coord) (dround coord 3))
                                      (collect-3d-vbap-data triplet)))
                 (get-3d-triplets speakers))))

;;; 2-d case:

(defun sort-2d-speakers (speakers)
  "speaker azimuths have to be reduced to [-180..180] degrees before
  calling this function."
  (sort speakers (lambda (v1 v2) (< (azi v1) (azi v2)))))

(defun speaker-2d-back-angle (sp1 sp2)
  "determine counterclockwise angle between two clockwise-sorted
  speakers (supplied angles in the interval [-180..180]), result in
  the interval [0..360]."
  (let ((azi1 (azi sp1))
        (azi2 (azi sp2)))
    (+ 360 (- azi1 azi2))))

;;; main function for 2-d: Return all pairs, which cover the whole
;;; 2-d speaker-arrangement. The speakers are sorted clockwise,
;;; starting at the center behind the listener. In case the
;;; counterclockwise angle of the first and last speaker is < 170
;;; degree, their panning will be included, resulting in the
;;; possibility to use azimuth values for 360 degrees. Otherwise
;;; reasonable panning is only possible at angles (clockwise) between
;;; the angles of the first and last speaker and strange things can
;;; happen at other angles.

(defun get-2d-pairs (speakers)
  (let* ((speakers (sort-2d-speakers speakers))
         (speaker-ring (if (< (speaker-2d-back-angle
                               (first speakers) (car (last speakers)))
                              170)
                           (append speakers (list (first speakers)))
                           speakers)))
    (mapcar #'list speaker-ring (rest speaker-ring))))

;;; similar to the 3d functions of the same name:

(defun collect-2d-vbap-data (pair)
  (let ((coords (get-coords pair))
        (ls-nos (get-ls-nos pair)))
    (apply #'append ls-nos
           (mtx-transpose (mtx-2d-inverse coords)))))

(defun get-2d-vbap-buffer-data (speakers)
  (apply #'append (list 2.0 (float (length speakers)))
         (mapcar (lambda (pair) (mapcar (lambda (coords) (dround coords 3))
                                   (collect-2d-vbap-data pair)))
                 (get-2d-pairs speakers))))

;;; api function for vbap: Different to the original
;;; function (define_loudspeakers in Max/pd and VBapSpeakerArray in
;;; supercollider), the dimension gets determined automatically based
;;; on the structure of the supplied argument.

(defun vbap-speaker-array (speaker-defs)
    "calculate the sets-and-matrices list used by the vbap ugen by
  providing a sequence of angles (2-d) or angle/elevation
  pairs (3-d). Elevation should be in the range [-90..90] (no checking
  is done!). The sequences have to be supplied as lists. The
  calculated seq needs to be stored in a buffer on the sc-server to be
  referenced by the vbap ugen.

  Examples:

  2-d: (vbap-speaker-array '(-45 0 45 90 135 180 -135 -90))

  3-d: (vbap-speaker-array
        '((-45 0) (0 45) (45 0) (90 45) (135 0) (180 45) (-135 0) (-90 45)))

  For complete usage examples see the documentation for the vbap ugen.
"
    (let ((speakers (get-speaker-maps speaker-defs)))
      (if (numberp (first speaker-defs))
          (get-2d-vbap-buffer-data speakers)
          (get-3d-vbap-buffer-data speakers))))


#|
;;; Examples:

(apply #'append
         (mapcar #'list
                 '(-45 0 45 90 135 180 -135 -90)
                 '(0 45 0 45 0 45 0 45)))

 => (-45 0 0 45 45 0 90 45 135 0 180 45 -135 0 -90 45)


(vbap-speaker-array '(-45 0 45 90 135 180 -135 -90))

(2.0 8.0 
7.0 8.0 
 -1.414 0.0 1.0 -1.0 
8.0 1.0 
 -1.0 -1.0 1.414 0.0 
1.0 2.0 
 0.0 -1.414 1.0 1.0 
2.0 3.0
 1.0 -1.0 0.0 1.414 
3.0 4.0
 1.414 0.0 -1.0 1.0 
4.0 5.0
 1.0 1.0 -1.414 0.0 
5.0 6.0
 0.0 1.414 -1.0 -1.0 
6.0 7.0 
-1.0 1.0 0.0 -1.414)

(vbap-speaker-array
 (mapcar #'list
         '(-45 0 45 90 135 180 -135 -90)
         '(0 45 0 45 0 45 0 45)))



(3.0 8.0

 1.0 2.0 3.0
 0.707 -0.707 -0.707 0.0 0.0 1.414 0.707 0.707 -0.707
 0.707 0.707 0.707 -0.707 0.0 0.707 0.0 0.707 0.0

 1.0 2.0 8.0
 0.707 -0.707 -0.707 0.707 0.707 0.707 -0.707 -0.707 0.707
 0.707 0.707 0.0 -0.707 0.0 -0.707 0.0 0.707 0.707

 1.0 7.0 8.0
 0.707 -0.707 -0.707 -0.707 -0.707 -0.707 0.0 0.0 1.414
 0.707 -0.707 0.0 -0.707 -0.707 -0.707 0.0 0.0 0.707

 2.0 3.0 4.0
 0.707 -0.707 0.707 0.707 0.707 -0.707 -0.707 0.707 0.707
 0.707 0.707 0.0 0.0 0.707 0.707 0.707 0.0 0.707

 2.0 4.0 6.0
 0.707 -0.707 0.707 0.0 1.414 0.0 -0.707 -0.707 0.707
 0.707 0.0 -0.707 0.0 0.707 0.0 0.707 0.707 0.707

 2.0 6.0 8.0
 0.707 0.707 0.707 -0.707 0.707 0.707 0.0 -1.414 0.0
 0.707 -0.707 0.0 0.0 0.0 -0.707 0.707 0.707 0.707

 3.0 4.0 5.0
 0.707 0.707 -0.707 0.0 0.0 1.414 -0.707 0.707 -0.707
 0.707 0.0 -0.707 0.707 0.707 0.707 0.0 0.707 0.0

 4.0 5.0 6.0
 0.707 0.707 0.707 -0.707 0.707 -0.707 -0.707 -0.707 0.707
 0.0 -0.707 -0.707 0.707 0.707 0.0 0.707 0.0 0.707

5.0 6.0 7.0
 -0.707 0.707 -0.707 0.0 0.0 1.414 -0.707 -0.707 -0.707
 -0.707 -0.707 -0.707 0.707 0.0 -0.707 0.0 0.707 0.0 

6.0 7.0 8.0
 -0.707 0.707 0.707 -0.707 -0.707 -0.707 0.707 -0.707 0.707
 -0.707 -0.707 0.0 0.0 -0.707 -0.707 0.707 0.0 0.707)

Output Pd Objekt:

3 8 

1 2 3 
 0.707107 -0.707107 -0.707107 -0 -0 1.41421 0.707107 0.707107 -0.707107
 0.707107 0.707107 0.707107 -0.707107 0 0.707107 0 0.707107 0 

1 2 8 
 0.707107 -0.707107 -0.707107 0.707107 0.707107 0.707107 -0.707107 -0.707107 0.707107 
 0.707107 0.707107 -3.09086e-08 -0.707107 0 -0.707107 0 0.707107 0.707107

1 7 8
 0.707107 -0.707107 -0.707107 -0.707107 -0.707107 -0.707107 -0 0 1.41421
 0.707107 -0.707107 -3.09086e-08 -0.707107 -0.707107 -0.707107 0 0 0.707107 

2 3 4 
 0.707107 -0.707107 0.707107 0.707107 0.707107 -0.707107 -0.707107 0.707107 0.707107 
 0.707107 0.707107 -3.09086e-08 0 0.707107 0.707107 0.707107 0 0.707107 

2 4 8 
 1.41421 -0 6.18172e-08 -0.707107 0.707107 0.707107 -0.707107 -0.707107 0.707107 
 0.707107 -3.09086e-08 -3.09086e-08 0 0.707107 -0.707107 0.707107 0.707107 0.707107 

3 4 5
 0.707107 0.707107 -0.707107 0 -0 1.41421 -0.707107 0.707107 -0.707107 0.707107 
 -3.09086e-08 -0.707107 0.707107 0.707107 0.707107 0 0.707107 0 

4 5 6
 0.707107 0.707107 0.707107 -0.707107 0.707107 -0.707107 -0.707107 -0.707107 0.707107
 -3.09086e-08 -0.707107 -0.707107 0.707107 0.707107 -6.18172e-08 0.707107 0 0.707107

4 6 8
 0.707107 0.707107 0.707107 -1.41421 0 -6.18172e-08 0.707107 -0.707107 0.707107
 -3.09086e-08 -0.707107 -3.09086e-08 0.707107 -6.18172e-08 -0.707107 0.707107 0.707107 0.707107

5 6 7
 -0.707107 0.707107 -0.707107 0 -0 1.41421 -0.707107 -0.707107 -0.707107 
 -0.707107 -0.707107 -0.707107 0.707107 -6.18172e-08 -0.707107 0 0.707107 0 

6 7 8
 -0.707107 0.707107 0.707107 -0.707107 -0.707107 -0.707107 0.707107 -0.707107 0.707107
 -0.707107 -0.707107 -3.09086e-08 -6.18172e-08 -0.707107 -0.707107 0.707107 0 0.707107

|#
