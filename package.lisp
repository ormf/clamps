;;; 
;;; package.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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



(defpackage :clamps
  (:export common-lisp-user:clamps clamps:clamps-base-url clamps:clamps-restart-gui
           clamps:set-bpm clamps:svg-gui-path clamps:set-tempo
           clamps:set-standard-pitch clamps:clamps-start
           clamps:meters clamps:*standard-pitch* clamps:idump
           common-lisp-user:clamps cl-midictl:gui
           clamps:clamps-gui-root clamps:start-doc-acceptor
           cm:reset-logger-stream cuda-usocket-osc:output-stream
           open output-stream-p stream input-stream-p
           cuda-usocket-osc:make-osc-receiver
           cuda-usocket-osc:stream-p
           cuda-usocket-osc:out-stream-open? close
           ats-cuda-display:ats->browser
           ats-cuda-display:restore-tables
           incudine.fudi:output-stream incudine.fudi:input-stream
           incudine.fudi:output-stream-p
           incudine.fudi:stream-open? incudine.fudi:send
           incudine.fudi:stream incudine.fudi:input-stream-p
           incudine.fudi:stream-p incudine.fudi:close
           incudine.fudi:open
           of-incudine-dsps:play-buffer-stretch-env-pan-out
           of-incudine-dsps:play-buffer-stretch-env-pan-out*
           of-incudine-dsps:play-buffer-stretch*
           of-incudine-dsps:buffer-stretch-play
           of-incudine-dsps:make-lsample
           of-incudine-dsps:get-keynum incudine.vug:counter
           of-incudine-dsps:play-buffer-stretch-out
           of-incudine-dsps:make-oasr
           of-incudine-dsps:play-buffer*
           of-incudine-dsps:*sine1024*
           of-incudine-dsps:play-sample*
           of-incudine-dsps:play-sample
           of-incudine-dsps:keynum->hz
           of-incudine-dsps:play-lsample
           of-incudine-dsps:*hanning1024*
           of-incudine-dsps:buffer-loop-play*
           of-incudine-dsps:play-buffer-stretch-env-out
           of-incudine-dsps:play-buffer-stretch
           of-incudine-dsps:line* of-incudine-dsps:phasor*
           of-incudine-dsps:restore-envs
           of-incudine-dsps:phasor-loop*
           of-incudine-dsps:play-lsample*
           of-incudine-dsps:lsample of-incudine-dsps:*env1*
           of-incudine-dsps:envelope*
           of-incudine-dsps:sfz->lsample of-incudine-dsps:osc~
           of-incudine-dsps:buffer-record
           incudine-bufs:get-sndfile-path
           incudine-bufs:add-buffer
           incudine-bufs:remove-all-buffers
           incudine-bufs:remove-buffer
           incudine-bufs:path-find-file incudine-bufs:get-buffer
           incudine-bufs:ensure-buffer incudine-bufs:buffer-id
           incudine-bufs:find-buffer incudine-bufs:of-buffer-load
           orm-utils:copy-instance orm-utils:ftom
           orm-utils:get-duplicates orm-utils:fibonacci
           orm-utils:port-available-p orm-utils:r-exp
           orm-utils:make-quantlist orm-utils:fr->ct
           orm-utils:sum_x orm-utils:map-proplist
           orm-utils:reverse-all orm-utils:n-exp-rev-fn
           orm-utils:mapply orm-utils:filter
           orm-utils:m-exp-zero-fn orm-utils:with-curr-dir
           orm-utils:rmprop orm-utils:map-indexed
           orm-utils:count-elements orm-utils:map-tree
           orm-utils:def-params orm-utils:system-version
           orm-utils:m-lin-fn orm-utils:m-lin-rev-fn
           orm-utils:n-lin-rev-fn orm-utils:mappend
           orm-utils:n-apply orm-utils:m-exp-rd-fn
           orm-utils:group-by orm-utils:flatten-fn
           orm-utils:reducing orm-utils:rfind orm-utils:ct2fv
           orm-utils:v-collect orm-utils:ct->fr orm-utils:exp-n
           orm-utils:lin-n orm-utils:with-lin-midi-fn
           orm-utils:db->amp orm-utils:rotate orm-utils:n-exp-fn
           orm-utils:call/collecting orm-utils:m-exp-dev
           orm-utils:str-concat orm-utils:delete-props
           orm-utils:n-collect orm-utils:do-proplist/collecting
           orm-utils:with-shadowed-variable orm-utils:group
           orm-utils:ntom orm-utils:n-lin-fn
           orm-utils:map-all-pairs orm-utils:slurp
           orm-utils:get-time orm-utils:cd orm-utils:calcsndbytes
           orm-utils:m-exp-rev-fn orm-utils:ucopy
           orm-utils:ensure-prop orm-utils:m-lin-dev
           orm-utils:m-exp-rd-rev-fn orm-utils:defvar*
           orm-utils:n-exp-dev orm-utils:m-exp-fn
           orm-utils:file-string orm-utils:repeat-format
           orm-utils:get-props-list orm-utils:get-prop
           orm-utils:permute orm-utils:range orm-utils:r-getf
           orm-utils:m-exp-zero-rev-fn orm-utils:differentiate
           orm-utils:repeat orm-utils:group-by-key
           orm-utils:n-exp orm-utils:slurp-string
           orm-utils:n-lin-dev orm-utils:make-keyword
           orm-utils:memorize-random-state
           orm-utils:parse-proplist orm-utils:date-string
           orm-utils:clip orm-utils:quantize-time
           orm-utils:map-params orm-utils:m-exp
           orm-utils:repeated orm-utils:with-exp-midi-fn
           orm-utils:r-exp-dev orm-utils:r-lin
           orm-utils:m-exp-zero orm-utils:n-lin orm-utils:r-elt
           orm-utils:index-list orm-utils:partition-seq
           orm-utils:spit orm-utils:defparameter* orm-utils:multf
           orm-utils:amp->db orm-utils:mtof orm-utils:splice
           orm-utils:all-permutations orm-utils:m-lin-rd-fn
           orm-utils:recall-random-state orm-utils:do-proplist
           orm-utils:randm orm-utils:mton orm-utils:subseqx
           orm-utils:n-exp-zero orm-utils:dround orm-utils:fv2ct
           orm-utils:with-props orm-utils:push-if
           orm-utils:setf-default orm-utils:let-default
           orm-utils:pwd orm-utils:do-repeated
           orm-utils:make-adjustable-string orm-utils:case-ext
           orm-utils:m-lin-rd-rev-fn orm-utils:combinations
           orm-utils:array-slice orm-utils:flatten
           orm-utils:m-lin orm-utils:every-nth
           cl-plot:construct-plot-command cl-plot:plot
           cl-plot:*gnuplot-header* cl-plot:*gnuplot-options*
           cl-plot:plot-2d cl-plot:with-gnuplot-instance
           cl-plot:*gnuplot-program* cl-plot:plot
           cl-refs:clear-dependencies cl-refs:bang-object
           cl-refs:trigger cl-refs:make-ref
           cl-refs:on-deps-update cl-refs:with-unwatched
           cl-refs:copy-ref cl-refs:make-bang
           cl-refs:make-computed cl-refs:watch cl-refs:set-val
           cl-refs:get-val cl-sfz:load-sfz-preset
           cl-sfz:sf-table-get-range cl-sfz:ensure-sfz-preset
           cl-sfz:list-sfz-presets cl-sfz:play-sfz-one-shot
           cl-sfz:sfz-preset-loaded? cl-sfz:get-sfz-preset
           cl-sfz:sfz-preset-buffer cl-sfz:play-sfz-loop
           cl-sfz:play-sfz cl-sfz:sfz-preset-file
           cl-sfz:add-sfz-preset cl-sfz:sfz-get-range
           cl-sfz:remove-sfz-preset cl-poolplayer:value-fn
           cl-poolplayer:collect-pool cl-poolplayer:set-basedir
           orm-utils:r-elt cl-poolplayer:get-dtime-fn
           cl-poolplayer:get-dtime-fn-no-x
           svg-import-export:new-id svg-import-export:transform
           svg-import-export:export-svg
           svg-import-export:format-with-slots
           svg-import-export:svg->lines
           svg-import-export:svg->points
           svg-import-export:points->svg
           svg-import-export:text-anchor
           svg-import-export:export-svg-file
           svg-import-export:list->svg-points
           svg-import-export:svg-collect-lines
           svg-import-export:svg-cm-line
           svg-import-export:regenerate-points
           svg-import-export:zoom svg-import-export:add-elements
           svg-import-export:svg-clone svg-import-export:svg-rect
           svg-import-export:make-cm-line
           svg-import-export:with-svg-file
           svg-import-export:lines->svg clog-dsp-widgets:flash
           clog-dsp-widgets:clear-bindings
           clog-dsp-widgets:db-slider->amp
           clog-dsp-widgets:create-o-slider
           clog-dsp-widgets:bind-refs-to-attrs
           clog-dsp-widgets:db-slider->db
           clog-dsp-widgets:start-gui clog-dsp-widgets:cuda-dsp
           clog-dsp-widgets:create-collection
           clog-dsp-widgets:create-hide-button
           clog-dsp-widgets:find-dsp clog-dsp-widgets:list-dsps
           clog-dsp-widgets:find-bus
           clog-dsp-widgets:create-o-numbox
           clog-dsp-widgets:db->db-slider
           clog-dsp-widgets:create-o-bang
           clog-dsp-widgets:dsp-nodes
           clog-dsp-widgets:new-gui-window
           clog-dsp-widgets:node-group
           clog-dsp-widgets:set-on-data
           clog-dsp-widgets:create-o-multislider
           clog-dsp-widgets:named-amp-bus
           clog-dsp-widgets:remove-bus
           clog-dsp-widgets:bus-channel
           clog-dsp-widgets:named-bus
           clog-dsp-widgets:amp->db-slider
           clog-dsp-widgets:master-amp-meter-bus
           clog-dsp-widgets:create-o-radio
           clog-dsp-widgets:*bindings*
           clog-dsp-widgets:remove-dsp clog-dsp-widgets:add-dsp
           clog-dsp-widgets:create-grid
           clog-dsp-widgets:clog-dsp-widgets-initialize
           clog-dsp-widgets:*out-refs* clog-dsp-widgets:bus-name
           clog-dsp-widgets:add-bus
           clog-dsp-widgets:create-o-vumeter
           clog-dsp-widgets:define-watch
           clog-dsp-widgets:levelmeter-gui
           clog-dsp-widgets:list-buses
           clog-dsp-widgets:bind-ref-to-attr
           clog-dsp-widgets:create-o-scope
           clog-dsp-widgets:num-chans clog-dsp-widgets:pulse-on
           clog-dsp-widgets:*in-refs*
           clog-dsp-widgets:create-o-toggle
           clog-dsp-widgets:create-o-knob
           clog-dsp-widgets:audio-bus
           clog-dsp-widgets:levelmeter-full-gui
           clog-dsp-widgets:highlight clog-dsp-widgets:pulse-off
           clog-dsp-widgets:master-amp-bus-levelmeter-gui
           clog-dsp-widgets:dsp-id clog-dsp-widgets:create-o-svg
           clog-dsp-widgets:master-bus-levelmeter-gui
           clog-dsp-widgets:levelmeter
           clog-dsp-widgets:remove-all-dsps
           clog-dsp-widgets:master-amp-out-levelmeter-gui
           cl-midictl:*midi-cc-state* cl-midictl:*oscin*
           cl-midictl:toggle-slot cl-midictl:set-marker
           cl-midictl:start-midi-receive
           cl-midictl:*midi-note-state*
           cl-midictl:update-all-controllers
           cl-midictl:stop-midi-receive
           cl-midictl:remove-midi-controller
           cl-midictl:*global-midi-channel*
           cl-midictl:with-gui-update-off
           cl-midictl:stop-osc-midi-receive cl-midictl:get-ref
           cl-midictl:handle-midi-in cl-midictl:midi-controller
           cl-midictl:pulse cl-midictl:midi-input
           cl-midictl:remove-all-midi-controllers
           cl-midictl:nanoktl2-midi
           cl-midictl:remove-all-midi-cc-fns
           cl-midictl:remove-all-channel-midi-cc-fns
           cl-midictl:osc-midi-write-short
           cl-midictl:faderfox-midi cl-midictl:update-state
           cl-midictl:*midi-debug* cm::chan
           cl-midictl:nanoktl2-preset-midi
           cl-midictl:show-midi-cc-fns cl-midictl:ccin
           cl-midictl:list-midi-controllers
           cl-midictl:find-controller cl-midictl:midi-output
           cl-midictl:*midi-note-fns*
           cl-midictl:add-midi-controller
           cl-midictl:start-midi-engine
           cl-midictl:update-preset-buttons
           cl-midictl:buchla-scale cl-midictl:remove-midi-cc-fns
           cl-midictl:gui cl-midictl:unwatch
           cl-midictl:add-midi-cc-fn
           cl-midictl:start-osc-midi-receive
           cl-midictl:*midi-cc-fns*
           clog-midi-controller:faderfox-gui
           clog-midi-controller:nanoktl2-gui
           clog-midi-controller:nanoktl2-preset-gui
           clog-midi-controller:midicontroller

           #:reset-logger-stream #:idump #:clamps #:clamps-restart-gui #:clamps-gui-root #:clamps-base-url #:set-standard-pitch #:*standard-pitch*
           #:svg-gui-path #:set-tempo #:set-bpm #:start-doc-acceptor #:clamps-start
           #:gui #:meters)
  (:shadowing-import-from #:incudine
                          #:rescale-envelope #:init #:stop
                          #:group #:*tempo* #:control-value
                          #:timestamp #:responder
;;; play-sample
                          )
  (:shadowing-import-from #:incudine.util
                          #:msg)

  (:shadowing-import-from #:of-incudine-dsps #:clip)
  (:shadowing-import-from #:cl-poolplayer #:args)

  (:use #:cl #:cl-user #:cm #:of-incudine-dsps #:ou)
  (:shadow #:mtof #:ftom
           #:buffer #:buffer-frames
           #:buffer-sample-rate
           #:node #:bus
           #:lsample #:envelope
           #:lsample-keynum #:lsample-play-fn #:lsample-amp #:lsample-buffer
           #:lsample-buffer #:remove-all-responders #:recv-stop
           #:without-interrupts
           #:play)

  (:shadowing-import-from #:cl-midictl
                          #:*midi-in1* #:*midi-out1*
                          #:chan #:id)

  (:shadowing-import-from #:cm #:pwd #:cd #:quantize #:at #:now #:tuning #:cycle #:rts?)
  (:shadowing-import-from #:ou #:range)
  (:shadowing-import-from #:cl-plot #:plot)
  (:use #:incudine
        #:incudine.util
        #:cl-midictl
        #:of-incudine-dsps
        #:incudine-bufs
        #:cl #:cl-refs
        #:cl-sfz
        #:clog-dsp-widgets
        #:ats-cuda
        #:ats-cuda-display
        #:clog-midi-controller
        #:cm
        ))
