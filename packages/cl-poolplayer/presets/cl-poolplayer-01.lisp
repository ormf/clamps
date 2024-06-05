(in-package :cl-poolplayer)

(progn
(digest-poolplayer-preset
0
(:p1 (funcall (getf args :p1 (lambda (x) x 0)) x)
:p2 (funcall (getf args :p2 (lambda (x) x 0)) x)
:p3 (funcall (getf args :p3 (lambda (x) x 0)) x)
:p4 (funcall (getf args :p4 (lambda (x) x 0)) x)
:dtimefn (funcall (getf args :dtimefn (lambda (x) x 0.5)) x)
:lsamplefn (funcall
            (getf args :lsamplefn (lambda (x) x (r-elt (getf args :g1)))) x)
:ampfn (funcall (getf args :ampfn (lambda (x) x -6)) x)
:transpfn (funcall (getf args :transpfn (lambda (x) x 0)) x)
:startfn (funcall (getf args :startfn (lambda (x) x 0)) x)
:endfn (funcall (getf args :endfn (lambda (x) x 0)) x)
:stretchfn (funcall (getf args :stretchfn (lambda (x) x 1)) x)
:wwidthfn (funcall (getf args :wwidthfn (lambda (x) x 123)) x)
:attackfn (funcall (getf args :attackfn (lambda (x) x 0)) x)
:panfn (funcall (getf args :panfn (lambda (x) x 0.5)) x)
:releasefn (funcall (getf args :releasefn (lambda (x) x 0.01)) x)
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
1
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
2
(:p1 (funcall (getf args :gidx) x)
:p2 (elt (getf args :g1) p1)
:p3 (elt p2 (round (* (1- (length p2)) (elt (getf args :plookup) p1))))
:p4 (+ (funcall (elt (getf args :transpfn) p1) x) (- 60 (first p3)))
:dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
:lsamplefn p3
:ampfn (funcall
        (or (getf args :ampfn)
            (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
        x)
:transpfn p4
:startfn 0
:endfn 0
:stretchfn (expt 2 (/ p4 -12))
:wwidthfn 123
:attackfn 0
:panfn (or (funcall (getf args :panfn) x) 0.5)
:releasefn 0.01
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
3
(:p1 (elt (getf args :g1) (funcall (getf args :poolselectfn) x))
:p2 (elt p1 (round (* (1- (length p1)) (funcall (getf args :poolposfn) x))))
:p3 (funcall (getf args :g2) x)
:p4 (float (+ (funcall (getf args :transpfn) x) (- 60 (lsample-keynum p2))) 1.0)
:dtimefn (r-exp (n-exp p3 0.01 0.3) (n-exp p3 0.01 3))
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:outfn (funcall (getf args :outfn #'stereo-out) x)))
(digest-poolplayer-preset
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
:stretchfn (r-exp 1 1)
:wwidthfn 123
:attackfn 0
:panfn 0.5
:releasefn 0.01
:outfn (funcall (getf args :outfn #'stereo-out) x)))
)
