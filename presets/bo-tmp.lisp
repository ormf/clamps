(in-package :big-orchestra)

;;; preset: 12

(digest-bo-preset
 12
 (:p1 (interp x 0 1 0.5 0 1 1)
  :p2 (random 48)
  :p3 (+ 0.1 (random 0.9))
  :p4 0
  :dtimefn (* (/ dur 5) (expt (/ 0.05 dur) p1))
  :bufferfn (getf args :bufferfn (r-elt *buffers*))
  :ampfn (+ 0 (* -12 p1))
  :transpfn (+ (expt p2 (- 1 p1)) (r-lin (- 1 (/ p2 2)) (- (/ p2 2))))
  :startfn 0
  :endfn 0.1
  :stretchfn (expt p3 x)
  :wwidthfn 123
  :attackfn 0
  :releasefn 0.01
  :outfn (funcall (getf args :outfn #'stereo-out) x)))

(save-presets)
