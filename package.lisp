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
  (:export cl-user:clamps
           
           standard-pitch
           svg-gui-root

           evt-amp
           evt-duration
           evt-keynum
           evt-time

           set-page-dimensions
           ats-cuda:load-ats
           ats-cuda:save-ats
           ats-cuda:track-ats
           ats-cuda:*ats-snd-dir*
           ats-cuda-display:ats->svg

           incudine-bufs:find-buffer
           incudine-bufs:ensure-buffer

           cl-sfz:sfz-preset-lsamples
           cl-sfz:sfz-preset-buffers
           cl-sfz:get-sfz-preset
           
           orm-utils:bool
           orm-utils:dolist-db
           orm-utils:ftom orm-utils:mtof
           orm-utils:get-props-list orm-utils:m-exp
           output-stream

           cl-midictl:midi-output cl-midictl:get-ref

           orm-utils:delete-props cl-midictl:buchla-scale
           orm-utils:call/collecting
           clog-dsp-widgets:levelmeter cl-midictl:nanoktl2-midi
           of-incudine-dsps:restore-envs orm-utils:m-lin
           cl-midictl:stop-midi-receive orm-utils:let-default
           clog-dsp-widgets:create-grid orm-utils:count-elements cl-refs:copy-ref
           cl-midictl:set-marker orm-utils:combinations export-svg
           orm-utils:with-curr-dir plot-2d
           orm-utils:differentiate of-incudine-dsps:buffer-loop-play*
           orm-utils:m-exp-rd-rev-fn orm-utils:map-params
           of-incudine-dsps:play-buffer-stretch-env-out orm-utils:make-quantlist
           set-basedir clamps-base-url orm-utils:slurp
           orm-utils:parse-proplist orm-utils:m-exp-fn
           orm-utils:r-lin-dev orm-utils:r-exp-dev clog-dsp-widgets:clog-dsp-widgets-initialize
           clog-dsp-widgets:new-gui-window
           clog-dsp-widgets:create-o-numbox orm-utils:amp->db
           cl-midictl:handle-midi-in
           ;; cm:clip
           clog-dsp-widgets:list-buses
           cl-midictl:show-midi-cc-fns orm-utils:ucopy orm-utils:get-prop
           clog-dsp-widgets:add-dsp orm-utils:do-proplist
           cl-sfz:ensure-sfz-preset get-dtime-fn input-stream
           export-svg-file incudine-bufs:remove-all-buffers
           cl-refs:make-computed *gnuplot-program* cl-sfz:add-sfz-preset
           format-with-slots cl-refs:on-deps-update
           clog-dsp-widgets:create-o-scope orm-utils:n-exp text-anchor
           orm-utils:n-lin-fn cl-midictl:add-midi-controller
           start-doc-acceptor orm-utils:reducing
           clog-dsp-widgets:amp->db-slider orm-utils:index-list
           clog-dsp-widgets:start-gui of-incudine-dsps:buffer-record
           cl-sfz:sfz-get-range of-incudine-dsps:*env1*
           cl-midictl:osc-midi-write-short value-fn
           clog-dsp-widgets:bus-channel clog-dsp-widgets:highlight
           cl-midictl:with-gui-update-off clog-dsp-widgets:cuda-dsp
           cl-midictl:*global-midi-channel* cm:pwd orm-utils:n-apply
           orm-utils:rfind collect-pool orm-utils:n-lin
           cl-sfz:sfz-preset-loaded? orm-utils:subseqx
           clog-dsp-widgets:unregister-element
           cl-midictl:update-preset-buttons clog-dsp-widgets:pulse-on
           ats-cuda-display:restore-tables orm-utils:make-keyword
           orm-utils:m-exp-dev orm-utils:fv2ct cl-midictl:*midi-cc-fns*
           incudine.util:logger-level
           reset-logger-stream meters clamps-gui-root
           incudine-bufs:buffer-id
           incudine-bufs:load-sounds-from-dir
           clog-dsp-widgets:pulse-off
           clog-dsp-widgets:*out-refs* orm-utils:rmprop orm-utils:multf
           of-incudine-dsps:play-buffer-stretch* orm-utils:r-lin idump
           mtof orm-utils:range clog-dsp-widgets:create-o-svg
           clog-midi-controller:nanoktl2-gui ats-cuda-display:ats->browser
           lines->svg orm-utils:m-exp-rev-fn clog-dsp-widgets:list-dsps
           common-lisp:input-stream-p list->svg-points
           orm-utils:str-concat orm-utils:m-exp-zero
           of-incudine-dsps:play-buffer-loop*
           of-incudine-dsps:play-buffer*
           *gnuplot-options* orm-utils:ct2fv svg-rect
           cl-midictl:*midi-note-fns* clog-dsp-widgets:bus-name orm-utils:ntom
           cl-midictl:stop-osc-midi-receive clog-dsp-widgets:db-slider->db
           cl-midictl:remove-midi-cc-fns
           clog-dsp-widgets:db->db-slider cl-refs:with-unwatched
           with-gnuplot-instance cl-sfz:play-sfz-one-shot
           cl-midictl:*midi-cc-state* clog-dsp-widgets:flash
           clog-dsp-widgets:named-amp-bus orm-utils:m-lin-fn
           incudine-bufs:add-buffer incudine-bufs:buffer-dur
           cl-midictl:faderfox-midi
           clog-dsp-widgets:bind-ref-to-attr orm-utils:array-slice
           orm-utils:with-props clog-dsp-widgets:remove-dsp
           cl-midictl:remove-all-midi-controllers *gnuplot-header*
           clog-dsp-widgets:num-channels of-incudine-dsps:envelope*
           cl-sfz:list-sfz-presets orm-utils:every-nth orm-utils:v-collect
           orm-utils:v-append
           clog-midi-controller:midicontroller clog-dsp-widgets:create-o-radio
           of-incudine-dsps:play-buffer-stretch-out
           common-lisp:stream orm-utils:r-exp orm-utils:all-permutations
           common-lisp:close of-incudine-dsps:phasor*
           clog-dsp-widgets:remove-all-dsps orm-utils:system-version
           cl-midictl:gui orm-utils:randm orm-utils:flatten
           orm-utils:m-exp-zero-fn construct-plot-command
           clog-dsp-widgets:create-hide-button orm-utils:m-lin-dev
           clog-dsp-widgets:dsp-nodes clog-dsp-widgets:audio-bus orm-utils:dround
           svg-clone cl-refs:clear-dependencies clog-dsp-widgets:*in-refs*
           cl-refs:bang-object cl-midictl:midi-controller cl-sfz:play-sfz
           cl-midictl:update-all-controllers
           clog-dsp-widgets:master-amp-bus-levelmeter-gui
           clog-dsp-widgets:bind-refs-to-attrs orm-utils:with-lin-midi-fn
           clog-dsp-widgets:master-amp-out-levelmeter-gui orm-utils:repeated
           of-incudine-dsps:phasor-loop* orm-utils:map-all-pairs
           out-stream-open? clog-dsp-widgets:master-bus-levelmeter-gui
           orm-utils:filter clog-dsp-widgets:clear-bindings zoom
           cl-midictl:remove-all-midi-cc-fns orm-utils:group-by
           regenerate-points with-svg-file cl-midictl:unwatch
           clog-dsp-widgets:define-watch orm-utils:port-available-p
           svg->lines orm-utils:m-lin-rd-fn
           cl-midictl:nanoktl2-preset-midi svg->points
           clog-dsp-widgets:remove-bus set-bpm orm-utils:case-ext
           orm-utils:push-if svg-cm-line orm-utils:splice
           orm-utils:quantize-time incudine.vug:counter cl-sfz:sfz-preset-file
           clog-dsp-widgets:add-bus clamps-start
           of-incudine-dsps:*hanning1024* orm-utils:lin-n orm-utils:def-params
           cl-midictl:start-osc-midi-receive clog-dsp-widgets:find-bus
           orm-utils:fr->ct
           of-incudine-dsps:play-buffer-stretch-env-pan-out*
           clog-midi-controller:nanoktl2-preset-gui orm-utils:db->amp
           orm-utils:do-proplist/collecting clog-dsp-widgets:levelmeter-full-gui
           cl-midictl:start-midi-receive orm-utils:r-getf orm-utils:partition-seq
           cl-refs:get-val orm-utils:n-lin-dev
           of-incudine-dsps:play-lsample orm-utils:with-exp-midi-fn
           transform clog-dsp-widgets:node-group
           orm-utils:memorize-random-state cl-sfz:play-sfz-loop
           of-incudine-dsps:play-buffer-stretch-env-pan-out add-elements
           orm-utils:calcsndbytes cl-sfz:load-sfz-preset common-lisp:open
           orm-utils:path-find-file clog-dsp-widgets:find-dsp ftom
           of-incudine-dsps:sfz->lsample orm-utils:do-repeated cl-midictl:pulse
           orm-utils:n-exp-dev make-osc-receiver svg-gui-path
           orm-utils:mapply clog-dsp-widgets:create-o-toggle
           incudine-bufs:remove-buffer incudine-bufs:clamps-buffer-load
           orm-utils:flatten-fn
           clog-dsp-widgets:set-on-data orm-utils:m-exp-zero-rev-fn send
           clog-dsp-widgets:dsp-id orm-utils:n-exp-rev-fn orm-utils:n-lin-rev-fn
           orm-utils:repeat-format cl-plot:plot orm-utils:defvar*
           of-incudine-dsps:play-buffer* orm-utils:ensure-prop orm-utils:r-elt
           points->svg clog-dsp-widgets:create-o-multislider
           orm-utils:recall-random-state orm-utils:date-string
           of-incudine-dsps:*sine1024* cl-midictl:ccin orm-utils:mton
           stream-open? cl-refs:make-ref
           of-incudine-dsps:play-buffer-stretch orm-utils:m-lin-rd-rev-fn
           orm-utils:map-tree orm-utils:map-proplist
           clog-dsp-widgets:create-o-vumeter svg-collect-lines
           common-lisp:output-stream-p orm-utils:sum_x
           cl-midictl:start-midi-engine
           cm::with cm::= cm::for cm::by cm::from cm::downto cm::to
           ;; cm::chan
           cl-refs:make-bang
           get-dtime-fn-no-x set-tempo stream-p
           orm-utils:reverse-all clog-dsp-widgets:master-amp-meter-bus
           clog-dsp-widgets:create-o-slider orm-utils:m-lin-rev-fn
           orm-utils:setf-default cl-midictl:list-midi-controllers
           cl-midictl:find-controller clog-dsp-widgets:named-bus
           cl-midictl:midi-input cl-midictl:toggle-slot of-incudine-dsps:osc~
           orm-utils:spit clog-dsp-widgets:levelmeter-gui orm-utils:n-exp-fn
           make-cm-line
           cl-midictl:*midi-note-state* of-incudine-dsps:make-lsample
           orm-utils:m-exp-rd-fn of-incudine-dsps:keynum->hz
           orm-utils:group-by-key new-id
           clog-dsp-widgets:create-collection cl-sfz:remove-sfz-preset
           orm-utils:mappend clog-dsp-widgets:*bindings* of-incudine-dsps:line*
           orm-utils:get-duplicates clog-dsp-widgets:create-o-bang
           orm-utils:n-exp-zero clog-dsp-widgets:db-slider->amp
           cl-sfz:sf-table-get-range
           cl-midictl:add-midi-cc-fn orm-utils:map-indexed
           orm-utils:with-shadowed-variable clog-dsp-widgets:create-o-knob
           clamps-restart-gui cl-refs:watch common-lisp-user:clamps
           cl-midictl:remove-midi-controller orm-utils:rotate
           orm-utils:make-adjustable-string of-incudine-dsps:get-keynum
           ;; cm:group
           cl-midictl:remove-all-channel-midi-cc-fns cm::repeat
           cm:cd orm-utils:fibonacci orm-utils:file-string
           orm-utils:exp-n orm-utils:defparameter* orm-utils:permute
           cl-midictl:update-hw-state orm-utils:copy-instance orm-utils:get-time
           cl-midictl:*midi-debug* orm-utils:slurp-string
           of-incudine-dsps:make-oasr cl-refs:set-val cl-midictl:*oscin*
           orm-utils:ct->fr clog-midi-controller:faderfox-gui cm:transp
           cm:play-midi cm:cm-store cm:midi-text-event cm:drunk
           cm:midi-chan-event cm:*midi-rcv-type-dummy*
           cm:*stream-recv-responders* cm:ctl-out cm:fold-objects cm:input
           cm:incudine-ensure-microtuning cm:sfz-oneshot cm:fudi-open
           cm:note-accidental cm:poolevt-amp cm:object-name cm:poolevt-lsample
           cm:poolevt-stretch cm:object-parameters cm:note-on cm:harmonics
           cm:weighting cm:invert cm:rescale-envelope cm:cm-version-number
           cm:midi-note-off cm:midi-port-event cm:clm-file
           cm:scale-shift-transform cm:append-object cm:poolevt-out2 cm:sco-file
           cm:sampleevt-keynum cm:fudi-open-default cm:*svg-colormap-old*
           cm:g-export cm:histogram cm:*softest* cm:map-subcontainers
           cm:poolevt-keynum cm:interpl cm:r-interpl cm:fudi cm:new-permutation
           cm:copy-object cm:player-unsolo cm:midi-smpte-offset cm:note-name
           cm:set-receiver! cm:events cm:remove-object cm:sfz-pan
           cm:incudine-stream cm:player-start cm:player-set-tempo
           cm:sampleevt-lsample cm:plotter-add-layer cm:chord
           cm:midi-open-default cm:player-save-midifile cm:pattern-value
           cm:plotter-property
           cm:samps->secs
           cm:+ml-channel-mask+ cm:ensure-jackmidi cm:range cm:*svg-x-scale*
           cm:midi-sequence-number cm:midi-tempo-change cm:pattern-state
           cm:quantize cm:pgm-change cm:scale-order cm:next cm:midi-channel-map
           cm:plotter-scroll cm:vstime->time-fn cm:decimals cm:heap cm:scale-min
           cm:accumulation cm:log-axis cm:plotter-data cm:pattern? cm:sv
           common-lisp-user:cm cm:samps->time cm:*loudest* cm:markov
           cm:midi-file-print cm:poolevt-pan cm:export-poolplayer-events
           cm:scale-max cm:imsg cm:process cm:midi-channel-pressure
           cm:sampleevt-out cm:prime-form cm:color->chan cm:ransegs cm:recv?
           cm:restart-qsynth cm:cd
           cm:pwd cm:secs->samps
           cm:rt-wait cm:defprocess cm:midi-system-event cm:insert-object cm:note
           cm:false cm:transpose-evt cm:set-clm-output-hook! cm:interp
           cm:amp->velo cm:ran cm:object-time cm:lsample->poolevt cm:keynum
           cm:opacity->db cm:vary cm:set-sco-output-hook! cm:scale<=
           cm:poolevt-end cm:chord-derive cm:copier cm:odds cm:*fudi.in*
           cm:time->vstime-fn cm:play-curr cm:time->speed-fn cm:pval
           cm:reset-logger-stream cm:cm-version cm:sfz-dur cm:play-fn
           cm:sfz-keynum cm:rt-proc cm:zero-shift cm:line cm:midi-key-pressure
           cm:amplitude cm:scale> cm:power cm:display cm:pitch-bend
           cm:sampleevt-amp cm:add-recreation-fn cm:plotter cm:plotter-redraw
           cm:reinit-midi cm:time->samps cm:status->channel cm:poolevt
           cm:permutation cm:poolevt-buffer-idx cm:transpose cm:write-event
           cm:args cm:preset cm:stop cm:region cm:find-object cm:pickl
           cm:cents->scaler cm:thunk cm:*midi-out1* cm:rescale cm:incudine-output
           cm:map-objects cm:sfz-amp cm:midi-stream cm:remove-subobjects cm:new
           cm:octave-number cm:pitch-class cm:poolevt-out1 cm:midi-out cm:jbmf
           cm:f cm:inkscape-export->cm cm:rewrite cm:*chromatic-scale*
           cm:note-off cm:midi-file cm:cmn-file cm:poolevt-wwidth cm:doeach
           cm:midi-note-on cm:drunk-traverse cm:make-mm-mask cm:subcontainers
           cm:sc-file cm:vstime->speed-fn cm:set-midi-output-hook! cm:scale<
           cm:start-cm-all cm:sprout cm:pick cm:rewrite-generation cm:svg->cm
           cm:player-stop cm:jackmidi-input-stream
           cm:stop-inkscape-osc
           cm:restart-inkscape-osc
           cm:recv-set! cm:svg->sfz cm:scaler->cents cm:sv* cm:save-object
           cm:lookup cm:play-svg cm:io cm:map-subobjects cm:svg-lines->cm
           cm:midi-key-signature cm:cycle
           cm:poolevt-release cm:at cm:now cm:axis cm:install-standard-sly-hooks
           cm:wait-until cm:plotter-close cm:seq cm:point cm:chan->color
           cm:scale-amp cm:poolevt-attack cm:output cm:player-mute cm:wait
           cm:midi-write-message
;;; cm:controller
           cm:expl ;;; cm:cm-restart-gui
           cm:start-inkscape-osc
           cm:*osc-inkscape-export-in* 
           cm:player-load-midifile cm:*svg-colormap*
           cm:svg->poolevt cm:explseg cm:list-subobjects cm:sfz-preset
           cm:midi-close-default cm:interval cm:midi-pitch-bend cm:rotation
           cm:*time-slots* cm:db->opacity cm:midi-note cm:song cm:subobjects
           cm:palindrome cm:i cm:reverse-obj cm:poolevt-start cm:player-unmute
           cm:defaxis cm:join cm:midi-connections cm:*fudi-out* cm:midi-eot
           cm:calc-dur cm:date-and-time cm:eod? cm:*midi-in1*
           cm:+ml-opcode-mask+ cm:sv+ cm:player-pause cm:best-normal-form
           cm:shell cm:jack-connect-qsynth cm:defobject cm:*beat* cm:mode
           cm:remove-receiver!
;;; cm:value
           cm:rts cm:sampleevt-start
           cm:status->opcode cm:make-mt-stream cm:cmn cm:tendency cm:sfz
           cm:decode-interval cm:hertz cm:object->cmn cm:rhythm cm:explsegs
           cm:midi cm:play cm:call-sly-connected-hooks cm:player-cont
           cm:*mt-out01* cm:true cm:audio-file cm:*rts-out* cm:between
           cm:fm-spectrum cm:fit cm:all-notes-off cm:rts-hush
           cm:add-rts-hush-hook cm:remove-all-rts-hush-hooks cm:show-rts-hush-hooks
           cm:midi-sequencer-event
           cm:*sly-connected-hooks* cm:sfz-startpos cm:graph cm:cmio cm:send-fudi
           cm:fudi-output-stream cm:player-solo cm:rt-sprout
           cm:list-named-objects cm:scale= cm:midi-time-signature
           cm:svg->sampleevt cm:map-pattern-data cm:*rt-scale* cm:dumposc
           cm:plotter-zoom cm:tuning cm:midi->incudine cm:make-cm cm:shuffle
           cm:jackmidi-output-stream cm:midi->sol cm:svfn
           cm:*midi-obj-name-dummy* cm:import-events cm:midi-control-change
           common-lisp:funcall cm:rm-spectrum cm:recv cm:scale-mod
           cm:markov-analyze cm:sampleevt cm:*fudi-in* cm:incudine-input
           cm:svg->browser cm:scale>= cm:fudi-close-default cm:transform-obj
           cm:eop? cm:transposer cm:midi-program-change cm:*tempo* cm:*scale*
           cm:plotter-front-styling cm:player-stream cm:scale-amplitude
           cuda-usocket-osc:output-stream cuda-usocket-osc:input-stream
           common-lisp:open common-lisp:output-stream-p cuda-usocket-osc:send
           common-lisp:stream common-lisp:input-stream-p
           cuda-usocket-osc:make-osc-receiver cuda-usocket-osc:stream-p
           cuda-usocket-osc:out-stream-open? common-lisp:close
           ats-cuda-display:atsd.idx ats-cuda-display:ats-display
           ats-cuda-display:atsd.mousepos ats-cuda-display:ats->browser
           ats-cuda-display:atsd.scale ats-cuda-display:atsd.shift-x
           ats-cuda-display:atsd.fmod ats-cuda-display:atsd.sound
           ats-cuda-display:pos-watch ats-cuda-display:atsd.amod
           ats-cuda-display:atsd.bw ats-cuda-display:atsd.data
           ats-cuda-display:play-watch ats-cuda-display:data-watch
           ats-cuda-display:atsd.crosshairs ats-cuda-display:atsd.play
           ats-cuda-display:atsd.width ats-cuda-display:restore-tables
           ats-cuda-display:ats-display-init ats-cuda-display:atsd.x
           ats-cuda-display:atsd.player-node-id ats-cuda-display:atsd.res-balance
           incudine.fudi:output-stream incudine.fudi:input-stream
           incudine.fudi:output-stream-p incudine.fudi:stream-open?
           incudine.fudi:send incudine.fudi:stream incudine.fudi:input-stream-p
           incudine.fudi:stream-p incudine.fudi:close incudine.fudi:open



           of-incudine-dsps:meters
           of-incudine-dsps:input-bus
           of-incudine-dsps:bus-value
           of-incudine-dsps:clear-buses
           of-incudine-dsps:cp-input-buses
           of-incudine-dsps:cp-output-buses
           of-incudine-dsps:mix-bus-to-out
           of-incudine-dsps:bus-to-out
           of-incudine-dsps:create-lsample
           of-incudine-dsps:play-buffer-stretch-env-pan-out
           of-incudine-dsps:play-buffer-stretch-env-pan-out*
           of-incudine-dsps:lsample-pathname
           of-incudine-dsps:lsample-dur
           of-incudine-dsps:lsample-name of-incudine-dsps:lsample-loopstart
           of-incudine-dsps:play-buffer-stretch*
           of-incudine-dsps:make-lsample
           of-incudine-dsps:get-keynum of-incudine-dsps:lsample-amp
           of-incudine-dsps:lsample-buffer incudine.vug:counter
           of-incudine-dsps:play-buffer-stretch-out of-incudine-dsps:make-oasr
           of-incudine-dsps:lsample-loopend
           of-incudine-dsps:*sine1024*
           of-incudine-dsps:play-sample of-incudine-dsps:keynum->hz
           of-incudine-dsps:play-lsample of-incudine-dsps:*hanning1024*
           of-incudine-dsps:buffer-loop-play* of-incudine-dsps:lsample-oneshot
           of-incudine-dsps:play-buffer-stretch-env-out
           of-incudine-dsps:play-buffer-stretch of-incudine-dsps:line*
           of-incudine-dsps:phasor* of-incudine-dsps:restore-envs
           of-incudine-dsps:phasor-loop*
           of-incudine-dsps:abs-path of-incudine-dsps:lsample
           of-incudine-dsps:*env1* of-incudine-dsps:envelope*
           of-incudine-dsps:sfz->lsample of-incudine-dsps:osc~
           of-incudine-dsps:buffer-record of-incudine-dsps:lsample-keynum incudine-bufs:add-buffer
           incudine-bufs:remove-all-buffers incudine-bufs:remove-buffer
           orm-utils:path-find-file
           incudine-bufs:ensure-buffer incudine-bufs:buffer-id
           incudine-bufs:find-buffer
           orm-utils:copy-instance orm-utils:ftom orm-utils:get-duplicates
           orm-utils:fibonacci orm-utils:port-available-p orm-utils:r-exp
           orm-utils:make-quantlist orm-utils:fr->ct orm-utils:sum_x
           orm-utils:map-proplist orm-utils:reverse-all orm-utils:n-exp-rev-fn
           orm-utils:mapply orm-utils:filter orm-utils:m-exp-zero-fn
           orm-utils:with-curr-dir orm-utils:rmprop orm-utils:map-indexed
           orm-utils:count-elements orm-utils:map-tree orm-utils:def-params
           orm-utils:system-version orm-utils:m-lin-fn orm-utils:m-lin-rev-fn
           orm-utils:n-lin-rev-fn orm-utils:mappend orm-utils:n-apply
           orm-utils:m-exp-rd-fn orm-utils:group-by orm-utils:flatten-fn
           orm-utils:reducing orm-utils:rfind orm-utils:ct2fv orm-utils:v-collect
           orm-utils:with-output-to-file orm-utils:ct->fr orm-utils:exp-n
           orm-utils:lin-n orm-utils:with-lin-midi-fn orm-utils:db->amp
           orm-utils:rotate orm-utils:n-exp-fn orm-utils:call/collecting
           orm-utils:m-exp-dev orm-utils:str-concat orm-utils:delete-props
           orm-utils:do-proplist/collecting
           orm-utils:format-time orm-utils:with-shadowed-variable orm-utils:group
           orm-utils:ntom orm-utils:n-lin-fn orm-utils:map-all-pairs
           orm-utils:slurp orm-utils:param-exp-func orm-utils:get-time
           orm-utils:cd orm-utils:calcsndbytes orm-utils:m-exp-rev-fn
           orm-utils:ucopy orm-utils:last-n orm-utils:ensure-prop
           orm-utils:m-lin-dev orm-utils:m-exp-rd-rev-fn orm-utils:defvar*
           orm-utils:n-exp-dev orm-utils:m-exp-fn orm-utils:file-string
           orm-utils:get-props-list orm-utils:get-prop
           orm-utils:permute orm-utils:range orm-utils:r-getf
           orm-utils:m-exp-zero-rev-fn orm-utils:differentiate orm-utils:repeat
           orm-utils:insert orm-utils:group-by-key orm-utils:n-exp
           orm-utils:slurp-string orm-utils:n orm-utils:n-lin-dev
           orm-utils:make-keyword orm-utils:memorize-random-state
           orm-utils:default orm-utils:parse-proplist orm-utils:date-string
           orm-utils:clip orm-utils:quantize-time orm-utils:map-params
           orm-utils:m-exp orm-utils:repeated orm-utils:with-exp-midi-fn
           orm-utils:r-exp-dev orm-utils:r-lin orm-utils:m-exp-zero
           orm-utils:n-lin orm-utils:r-elt orm-utils:index-list
           orm-utils:partition-seq orm-utils:spit orm-utils:defparameter*
           orm-utils:multf orm-utils:amp->db orm-utils:mtof orm-utils:splice
           orm-utils:all-permutations orm-utils:m-lin-rd-fn
           orm-utils:recall-random-state orm-utils:do-proplist orm-utils:randm
           orm-utils:mton orm-utils:subseqx orm-utils:n-exp-zero
           orm-utils:integrate orm-utils:dround orm-utils:fv2ct
           orm-utils:defconst orm-utils:with-props orm-utils:push-if
           orm-utils:setf-default orm-utils:let-default orm-utils:pwd
           orm-utils:do-repeated orm-utils:make-adjustable-string
           orm-utils:case-ext orm-utils:m-lin-rd-rev-fn orm-utils:combinations
           orm-utils:array-slice orm-utils:flatten orm-utils:m-lin
           orm-utils:every-nth cl-plot:construct-plot-command cl-plot:plot
           cl-plot:*gnuplot-header* cl-plot:*gnuplot-options*
           cl-plot:with-gnuplot-instance
           cl-plot:*gnuplot-program* cl-plot:plot cl-refs:ref-object-super
           cl-refs:clear-dependencies cl-refs:*refs-seen* cl-refs:bang-object
           cl-refs:ref-object cl-refs:trigger cl-refs:%trigger
           cl-refs:toggle-ref-fn
           cl-refs:add-trigger-fn cl-refs:remove-trigger-fn
           cl-refs:remove-all-triggers
           cl-refs:make-ref
           cl-refs:%set-val cl-refs:on-deps-update cl-refs:with-unwatched
           cl-refs:copy-ref cl-refs:make-bang cl-refs:make-computed cl-refs:watch
           cl-refs:trigger-fns cl-refs:ref-id cl-refs:ref-listeners cl-refs:set-val cl-refs:get-val
           cl-refs:with-updating-deps cl-sfz:load-sfz-preset
           cl-sfz:sf-table-get-range cl-sfz:ensure-sfz-preset
           cl-sfz:list-sfz-presets cl-sfz:play-sfz-one-shot
           cl-sfz:sfz-preset-loaded? cl-sfz:get-sfz-preset
           cl-sfz:play-sfz-loop cl-sfz:play-sfz
           cl-sfz:sfz-preset-file cl-sfz:add-sfz-preset cl-sfz:sfz-get-range
           cl-sfz:remove-sfz-preset cl-poolplayer:p1 cl-poolplayer:p-song-afterfn
           cl-poolplayer:*pool-hash* cl-poolplayer:init-poolplayer
           cl-poolplayer:preset-play cl-poolplayer:p2 cl-poolplayer:dtime
           cl-poolplayer:value-fn cl-poolplayer:*outseq8* cl-poolplayer:p4
           cl-poolplayer:save-poolplayer-presets
           cl-poolplayer:*poolplayer-presets-file*
           cl-poolplayer:digest-poolplayer-preset cl-poolplayer:g4
           cl-poolplayer:*circle-cw* cl-poolplayer:make-p-song
           cl-poolplayer:npreset-play cl-poolplayer:set-poolplayer-preset-form
           cl-poolplayer:stereo-out cl-poolplayer:dtime-dev
           cl-poolplayer:*outseq9* cl-poolplayer:collecting-cm
           cl-poolplayer:p-song-beforefn cl-poolplayer:perform
           cl-poolplayer:serialize-score cl-poolplayer:distributed-play
           cl-poolplayer:*poolplayer-events* cl-poolplayer:collect-pool
           cl-poolplayer:cm-collect cl-poolplayer:eventplotter
           cl-poolplayer:*outseq13* cl-poolplayer:eventplayer
           cl-poolplayer:load-poolplayer-presets cl-poolplayer:set-basedir
           cl-poolplayer:previous-poolplayer-preset cl-poolplayer:args
           cl-poolplayer:expand-arg-forms cl-poolplayer:x
           cl-poolplayer:show-poolplayer-preset cl-poolplayer:p-song-playfn
           cl-poolplayer:g1 cl-poolplayer:p-song-durfn orm-utils:r-elt
           cl-poolplayer:next-poolplayer-preset cl-poolplayer:p3 cl-poolplayer:g2
           cl-poolplayer:buf-idx cl-poolplayer:g3 cl-poolplayer:get-dtime-fn
           cl-poolplayer:play-song cl-poolplayer:p-song-name
           cl-poolplayer:fig12-out cl-poolplayer:fn-digest-poolplayer-preset
           cl-poolplayer:load-poolplayer-sounds cl-poolplayer:cm-collect-song
           cl-poolplayer:get-dtime-fn-no-x
           svg-import-export:add-svg-attr-props-to-quote
           svg-import-export:print-head-to-stream svg-import-export:opacity
           svg-import-export:w-x svg-import-export:new-id
           svg-import-export:svg-cm-line-attribute svg-import-export:d
           svg-import-export:transform svg-import-export:export-svg
           svg-import-export:svg-cm-line-x1 svg-import-export:id
           svg-import-export:marker-end svg-import-export:svg-barlines
           svg-import-export:svg-layer svg-import-export:visible
           svg-import-export:svg-cm-line-y1 svg-import-export:format-with-slots
           svg-import-export:svg-point svg-import-export:attributes
           svg-import-export:xscale-points svg-import-export:svg->lines
           svg-import-export:xscale-lines svg-import-export:x1
           svg-import-export:make-svg-cm-line
           svg-import-export:svg-cm-line-opacity svg-import-export:svg->points
           svg-import-export:stroke-width svg-import-export:w-width
           svg-import-export:label svg-import-export:x
           svg-import-export:points->svg svg-import-export:text-anchor
           svg-import-export:svg-text svg-import-export:export-svg-file
           svg-import-export:w-height svg-import-export:font-style
           svg-import-export:stroke-miterlimit1 svg-import-export:name
           svg-import-export:cx svg-import-export:last-id svg-import-export:rx
           svg-import-export:inverse svg-import-export:list->svg-points
           svg-import-export:height svg-import-export:y2
           svg-import-export:font-weight svg-import-export:svg-file
           svg-import-export:stroke-miterlimit svg-import-export:href
           svg-import-export:print-tail-to-stream svg-import-export:cy
           svg-import-export:fill-opacity svg-import-export:svg-collect-lines
           svg-import-export:ry svg-import-export:svg-cm-line
           svg-import-export:regenerate-points svg-import-export:zoom
           svg-import-export:stroke-linecap svg-import-export:stroke-color
           svg-import-export:w-y svg-import-export:svg-line
           svg-import-export:add-elements svg-import-export:elements
           svg-import-export:insensitive svg-import-export:fill-color
           svg-import-export:svg-cm-line-x2 svg-import-export:font-size
           svg-import-export:y svg-import-export:svg-color->pd-color
           svg-import-export:y1 svg-import-export:header
           svg-import-export:svg-clone svg-import-export:stroke-dasharray
           svg-import-export:gridtype svg-import-export:width
           svg-import-export:showgrid svg-import-export:svg-rect
           svg-import-export:pd-color->svg-color svg-import-export:renew-svg
           svg-import-export:make-staff-system svg-import-export:make-cm-line
           svg-import-export:svg-zeitachse svg-import-export:svg-staff-system
           svg-import-export:svg-cm-line-y2 svg-import-export:x2
           svg-import-export:font-family svg-import-export:stroke-opacity
           svg-import-export:with-svg-file svg-import-export:svg-group
           svg-import-export:fill-rule svg-import-export:fname
           svg-import-export:id-hash svg-import-export:svg-class
           svg-import-export:stroke-linejoin svg-import-export:lines->svg
           svg-import-export:get-tick-lines svg-import-export:svg-piano-roll
           svg-import-export:svg-cm-line-color svg-import-export:print-to-stream
           svg-import-export:make-piano-roll clog-dsp-widgets:flash
           clog-dsp-widgets:clear-bindings clog-dsp-widgets:db-slider->amp
           clog-dsp-widgets:b-map clog-dsp-widgets:create-o-slider
           clog-dsp-widgets:bind-refs-to-attrs clog-dsp-widgets:db-slider->db
           clog-dsp-widgets:start-gui clog-dsp-widgets:cuda-dsp
           clog-dsp-widgets:create-collection clog-dsp-widgets:num-meters
           clog-dsp-widgets:amp-node clog-dsp-widgets:create-hide-button
           clog-dsp-widgets:find-dsp clog-dsp-widgets:list-dsps
           clog-dsp-widgets:find-bus clog-dsp-widgets:create-o-numbox
           clog-dsp-widgets:db->db-slider clog-dsp-widgets:create-o-bang
           clog-dsp-widgets:dsp-nodes clog-dsp-widgets:new-gui-window
           clog-dsp-widgets:node-group clog-dsp-widgets:set-on-data
           clog-dsp-widgets:create-o-multislider clog-dsp-widgets:named-amp-bus
           clog-dsp-widgets:remove-bus clog-dsp-widgets:bus-channel
           clog-dsp-widgets:named-bus clog-dsp-widgets:amp->db-slider
           clog-dsp-widgets:master-amp-meter-bus clog-dsp-widgets:b-elist
           clog-dsp-widgets:b-unwatch clog-dsp-widgets:refs
           clog-dsp-widgets:create-o-radio clog-dsp-widgets:*bindings*
           clog-dsp-widgets:b-unregister clog-dsp-widgets:remove-dsp
           clog-dsp-widgets:add-dsp clog-dsp-widgets:binding-name
           clog-dsp-widgets:create-grid clog-dsp-widgets:num-channels
           clog-dsp-widgets:b-ref clog-dsp-widgets:clog-dsp-widgets-initialize
           clog-dsp-widgets:*out-refs* clog-dsp-widgets:bus-name
           clog-dsp-widgets:add-bus clog-dsp-widgets:create-o-vumeter
           clog-dsp-widgets:define-watch clog-dsp-widgets:levelmeter-gui
           clog-dsp-widgets:list-buses clog-dsp-widgets:bind-ref-to-attr
           clog-dsp-widgets:binding clog-dsp-widgets:create-o-scope
           clog-dsp-widgets:pulse-on clog-dsp-widgets:b-attr
           clog-dsp-widgets:*in-refs* clog-dsp-widgets:create-o-toggle
           clog-dsp-widgets:create-o-knob clog-dsp-widgets:audio-bus
           clog-dsp-widgets:levelmeter-full-gui clog-dsp-widgets:highlight
           clog-dsp-widgets:pulse-off
           clog-dsp-widgets:master-amp-bus-levelmeter-gui
           clog-dsp-widgets:channel-offs clog-dsp-widgets:dsp-id
           clog-dsp-widgets:format-style clog-dsp-widgets:create-o-svg
           clog-dsp-widgets:master-bus-levelmeter-gui clog-dsp-widgets:levelmeter
           clog-dsp-widgets:opt-format-attr clog-dsp-widgets:remove-all-dsps
           clog-dsp-widgets:master-amp-out-levelmeter-gui
           cl-midictl:*midi-cc-state* cl-midictl:nk2-fader-modes
           cl-midictl:*oscin* cl-midictl:toggle-slot cl-midictl:set-marker
           cl-midictl:start-midi-receive cl-midictl:*midi-note-state*
           cl-midictl:update-all-controllers cl-midictl:echo cl-midictl:r-buttons
           cl-midictl:m-buttons cl-midictl:curr-bank cl-midictl:note-state
           cl-midictl:stop-midi-receive cl-midictl:remove-midi-controller
           cl-midictl:curr-player cl-midictl:presets
           cl-midictl:digest-nanoktl2-presets cl-midictl:*global-midi-channel*
           cl-midictl:with-gui-update-off cl-midictl:stop-osc-midi-receive
           cl-midictl:get-ref cl-midictl:gui-update-off
           cl-midictl:get-active-players cl-midictl:handle-midi-in
           cl-midictl:cc-nums cl-midictl:tr-stop cl-midictl:nk2-fader-last-cc
           cl-midictl:midi-controller cl-midictl:*curr-preset*
           cl-midictl:button-labels cl-midictl:pulse cl-midictl:tr-ffwd
           cl-midictl:preset-buttons cl-midictl:handle-player-switch
           cl-midictl:midi-input cl-midictl:remove-all-midi-controllers
           cl-midictl:cycle cl-midictl:nanoktl2-midi cl-midictl:marker-left
           cl-midictl:remove-all-midi-cc-fns
           cl-midictl:remove-all-channel-midi-cc-fns cl-midictl:nk-cycle
           cl-midictl:osc-midi-write-short cl-midictl:faderfox-midi
           cl-midictl:preset-state
           cl-midictl:faderfox-midi-f.orm cl-midictl:*midi-debug*
           cl-midictl:cc-map cl-midictl:handle-store-button-press
           cl-midictl:s-buttons cl-midictl:nanoktl2-preset-midi
           cl-midictl:show-midi-cc-fns cl-midictl:ccin cl-midictl:cc-state
           cl-midictl:init-nk2 cl-midictl:list-midi-controllers
           cl-midictl:nk2-faders cl-midictl:find-controller
           cl-midictl:handle-player-button-press cl-midictl:midi-output
           cl-midictl:last-note-on cl-midictl:*midi-note-fns*
           cl-midictl:add-midi-controller cl-midictl:note-fns
           cl-midictl:marker-right cl-midictl:tr-play
           cl-midictl:start-midi-engine cl-midictl:update-preset-buttons
           cl-midictl:handle-preset-button-press cl-midictl:track-right
           cl-midictl:buchla-scale cl-midictl:*nanoktl2-presets-file*
           cl-midictl:cc-fns cl-midictl:save-presets cl-midictl:load-presets
           cl-midictl:nk2-last-cc cl-midictl:remove-midi-cc-fns
           cl-midictl:bank-buttons cl-midictl:gui cl-midictl:set-player-buttons
           cl-midictl:cp-src cl-midictl:tr-rewind cl-midictl:unwatch
           cl-midictl:track-left cl-midictl:nk2-fader-update-fns
           cl-midictl:add-midi-cc-fn cl-midictl:start-osc-midi-receive
           cl-midictl:hide-fader cl-midictl:*midi-cc-fns* cl-midictl:tr-rec
           clog-midi-controller:gui-set-marker clog-midi-controller:gui-rewind
           clog-midi-controller:gui-play clog-midi-controller:gui-ffwd
           clog-midi-controller:gui-container clog-midi-controller:gui-s-buttons
           clog-midi-controller:gui-marker-left
           clog-midi-controller:gui-ctl-panel clog-midi-controller:faderfox-gui
           clog-midi-controller:gui-track-left clog-midi-controller:nanoktl2-gui
           clog-midi-controller:gui-rec clog-midi-controller:gui-marker-right
           clog-midi-controller:gui-stop clog-midi-controller:gui-m-buttons
           clog-midi-controller:gui-cycle
           clog-midi-controller:nanoktl2-preset-gui
           clog-midi-controller:midicontroller clog-midi-controller:gui-r-buttons
           clog-midi-controller:gui-track-right clog-midi-controller:gui-fader
           clog-midi-controller:gui-parent clog-midi-controller:ctl-panel-vis
           cm:tempo->svg-timescale)
  (:shadowing-import-from #:incudine
                          #:rescale-envelope #:init #:stop
                          #:group #:*tempo* #:control-value
                          #:timestamp #:responder
;;; play-sample
                          )
  (:shadowing-import-from #:incudine.util
                          #:msg)

  (:shadowing-import-from #:clog
                          #:title #:style #:html-document
                          #:set-on-new-window)

  (:shadowing-import-from #:of-incudine-dsps #:clip)
  (:shadowing-import-from #:cl-poolplayer #:args)

  (:use #:cl #:cl-user #:cm #:of-incudine-dsps #:ou)
  (:shadow #:buffer #:buffer-frames
           #:buffer-sample-rate
           #:node #:bus
           #:envelope
           #:remove-all-responders #:recv-stop
           #:without-interrupts
           #:play)

  (:shadowing-import-from #:cl-midictl
                          #:*midi-in1* #:*midi-out1*
                          #:chan #:id)
  (:shadowing-import-from #:cm #:args #:cycle
                          #:tempo->svg-timescale
                          #:with #:initially #:finally #:repeat #:for
                          #:do #:each #:output #:sprout #:while #:until #:when #:unless #:if
                          #:and #:= #:then #:from #:to #:downto #:below #:in #:on #:by #:over #:of #:set #:wait
                          #:duration #:time #:keynum #:amplitude #:channel #:stop #:at #:quantize
                          #:*tempo* #:tuning #:now #:rescale-envelope #:poolevt
                          #:pwd #:cd #:rts?)
  
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
        #:orm-utils
        #:clog-dsp-widgets
        ))
