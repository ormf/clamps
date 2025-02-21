(in-package :cl-poolplayer)

(progn
  (set-poolplayer-preset-form
   0
   (:p1 *mde-pool-02*
    :p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
    :p3 0
    :p4 (float
         (- (* 127 (+ (ccin 10) (- (/ (ccin 11) 2) (r-lin 0 (ccin 11)))))
            (lsample-keynum p2))
         1.0)
    :dtimefn (* (n-exp (ccin 8) 5 0.02) (n-exp-dev (ccin 9) 10))
    :lsamplefn p2
    :ampfn (+ (random 12) -6 (n-lin (ccin 6) -40 0))
    :transpfn p4
    :startfn 0
    :endfn 0
    :stretchfn (expt 2 (/ p4 -12))
    :wwidthfn 12
    :attackfn 0
    :panfn (random 1.0)
    :releasefn 0.01
    :outfn (stereo-out)))
  (set-poolplayer-preset-form
   1
   (:p1 *vc-wal*
    :p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
    :p3 0
    :p4 (float
         (- (* 127 (+ (ccin 10) (- (/ (ccin 11) 2) (r-lin 0 (ccin 11)))))
            (lsample-keynum p2))
         1.0)
    :dtimefn (cm::clip (* (n-exp (ccin 8) 5 0.02) (n-exp-dev (ccin 9) 10)) 0.1 10)
    :lsamplefn p2
    :ampfn (+ (random 12) -6 (n-lin (ccin 6) -40 0))
    :transpfn p4
    :startfn 0
    :endfn 0
    :stretchfn (* (n-exp (ccin 13) 1 100) (expt 2 (/ p4 -12)))
    :wwidthfn 123
    :attackfn 0
    :panfn (random 1.0)
    :releasefn 0.01
    :outfn (stereo-out)))
  (set-poolplayer-preset-form
   2
   (:p1 *oud-wischen*
    :p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
    :p3 0
    :p4 (float
         (- (* 127 (+ (ccin 10) (- (/ (ccin 11) 2) (r-lin 0 (ccin 11)))))
            (lsample-keynum p2))
         1.0)
    :dtimefn (* (n-exp (ccin 8) 5 0.02) (n-exp-dev (ccin 9) 10))
    :lsamplefn p2
    :ampfn (+ (random 12) -6 (n-lin (ccin 6) -40 0))
    :transpfn p4
    :startfn 0
    :endfn 0
    :stretchfn (expt 2 (/ p4 -12))
    :wwidthfn 12
    :attackfn 0
    :panfn (random 1.0)
    :releasefn 0.01
    :outfn (stereo-out)))
  (set-poolplayer-preset-form
   3
   (:p1 *pno-oud-klopfen*
    :p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
    :p3 0
    :p4 (float
         (- (+ (ccin 9) (- (/ (ccin 11) 2) (r-lin 0 (ccin 11))))
            (lsample-keynum p2))
         1.0)
    :dtimefn (cm::clip (* (n-exp (ccin 8) 5 0.02) (n-exp-dev (ccin 9) 10)) 0.01 10)
    :lsamplefn p2
    :ampfn (+ (random 12) -6 (n-lin (ccin 6) -40 0))
    :transpfn p4
    :startfn 0
    :endfn 0
    :stretchfn (* 1 (expt 2 (/ p4 -12)))
    :wwidthfn 123
    :attackfn (n-lin (ccin 13) 0 1)
    :panfn (random 1.0)
    :releasefn 0.01
    :outfn (stereo-out)))
  (set-poolplayer-preset-form
   4
   (:p1 (elt (getf args :g1) (funcall (getf args :poolselectfn) x))
    :p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
    :p3 (funcall (getf args :g2))
    :p4 (float (+ (funcall (getf args :transpfn) x) (- 60 (lsample-keynum p2))) 1.0)
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn p2
    :ampfn (+ (or (getf args :amp 0))
              (funcall (or (getf args :ampfn) (lambda (x) x (+ (random 12) -6))) x))
    :transpfn p4
    :startfn 0
    :endfn 0
    :stretchfn (expt 2 (/ p4 -12))
    :wwidthfn 123
    :attackfn 0
    :panfn (or (funcall (getf args :panfn) x) 0.5)
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   5
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   6
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   7
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   8
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   9
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   10
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   11
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   12
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   13
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   14
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   15
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   16
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   17
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   18
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   19
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   20
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   21
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   22
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   23
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   24
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   25
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   26
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   27
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   28
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   29
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   30
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   31
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   32
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   33
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   34
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   35
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   36
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   37
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   38
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   39
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   40
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   41
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   42
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   43
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   44
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   45
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   46
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   47
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   48
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   49
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   50
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   51
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   52
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   53
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   54
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   55
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   56
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   57
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   58
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   59
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   60
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   61
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   62
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   63
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   64
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   65
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   66
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   67
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   68
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   69
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   70
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   71
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   72
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   73
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   74
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   75
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   76
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   77
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   78
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   79
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   80
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   81
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   82
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   83
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   84
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   85
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   86
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   87
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   88
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   89
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   90
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   91
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   92
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   93
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   94
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   95
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   96
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   97
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   98
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1)
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  (set-poolplayer-preset-form
   99
   (:p1 0
    :p2 0
    :p3 0
    :p4 0
    :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
    :lsamplefn (r-elt (getf args :g1))
    :ampfn (funcall
            (or (getf args :ampfn)
                (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
            x)
    :transpfn (funcall
               (getf args :transpfn
                     (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
               x)
    :startfn 0
    :endfn 0
    :stretchfn (r-exp 1 1) y
    :wwidthfn 123
    :attackfn 0
    :panfn 0.5
    :releasefn 0.01
    :outfn (getf args :outfn (stereo-out))))
  )
