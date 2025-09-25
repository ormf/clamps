(in-package :clamps)

(progn
(set-poolplayer-preset-form
0
(:bindings ((transp (+ 5 (random 10.0))))
:dtimefn (r-exp 0.05 0.4)
:lsamplefn (r-elt *pool0*)
:ampfn -6
:durfn dtime
:transpfn transp
:startfn 0
:endfn 0
:stretchfn (expt 2 (/ transp -12))
:wwidthfn 1
:attackfn 0
:panfn (random 1.0)
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
1
(:bindings nil
:dtimefn 0.1
:lsamplefn (r-elt *pool0*)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
2
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
3
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
4
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
5
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
6
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
7
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
8
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
9
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
10
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
11
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
12
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
13
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
14
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
15
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
16
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
17
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
18
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
19
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
20
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
21
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
22
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
23
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
24
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
25
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
26
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
27
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
28
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
29
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
30
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
31
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
32
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
33
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
34
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
35
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
36
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
37
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
38
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
39
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
40
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
41
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
42
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
43
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
44
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
45
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
46
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
47
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
48
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
49
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
50
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
51
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
52
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
53
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
54
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
55
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
56
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
57
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
58
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
59
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
60
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
61
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
62
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
63
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
64
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
65
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
66
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
67
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
68
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
69
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
70
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
71
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
72
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
73
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
74
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
75
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
76
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
77
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
78
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
79
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
80
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
81
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
82
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
83
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
84
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
85
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
86
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
87
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
88
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
89
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
90
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
91
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
92
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
93
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
94
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
95
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
96
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
97
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
98
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
(set-poolplayer-preset-form
99
(:bindings ((cl-poolplayer::pool (getf args :g1))
            (lsample (r-elt cl-poolplayer::pool)))
:dtimefn 0.5
:bufferfn (lsample-buffer lsample)
:ampfn -6
:transpfn 0
:startfn 0
:endfn 0
:stretchfn 1
:wwidthfn 1
:attackfn 0
:panfn 0
:releasefn 0.01
:outfn (stereo-out)))
)
