;;; **********************************************************************
;;; Copyright (C) 2006 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Name$
;;; $Revision$
;;; $Date$

;;;
;;; Emacs/Slime support for Common Music. Most commands are not bound
;;; to keys, see entries for some suggestions. CM's documentation menu
;;; is installed under the SLIME -> Documentation submenu.
;;;
;;; M-x cm                                                       [Command]
;;;   Start up cm in a new frame, window, or buffer (see
;;;   cm-command and inferior-lisp-display below).
;;; M-x kill-cm                                                  [Command]
;;;   Kill existing *slime-repl* session.
;;; M-x enable-cm-commands                                       [Command]
;;;   Adds the following keyboard comands:
;;;   <f8>      One-stroke switching between repl and lisp buffer.
;;;   C-cC-dc   Find symbol at point in Common Music dictionary. 
;;;             (also in menu SLIME->Documentation->Common Music)
;;;   C-xC-e    Eval expr before, after, around or whole region.
;;;             On OS X this is also installed on APPLE-E.
;;;   TAB       Indent line, region or defun (if prefixed).
;;;
;;; *common-music-doc-root*                                     [Variable]
;;;   The root URL for browsing CM documentation. Defaults
;;;   to "http://commonmusic.sf.net/doc/"
;;; cm-program
;;;   The shell program to start CM. Defaults to "cm".
;;; cm-systems
;;;   A list of systems to load when CM starts.
;;; cm-scratch-mode
;;;   Emacs edit mode for *scratch* buffer, one of: lisp, sal or nil.

;; (unless (member 'slime features)
;;   (require 'slime)
;;   (slime-setup))

(require 'cl-lib)

(when (member 'aquamacs features)
  (add-to-list 'obof-other-frame-regexps " \\*inferior-lisp\\*")
  (add-to-list 'obof-other-frame-regexps "\\*slime-repl\\\\*"))

;; update default value of inferior-lisp-program to "cm.sh"

(defvar cm-program
  (if (or (not (boundp 'inferior-lisp-program))
	  (not inferior-lisp-program)
	  (equal inferior-lisp-program "lisp"))
      (or (locate-library "bin/cm.sh" t)
	  "cm")))

(defvar cm-systems (list))

(defvar cm-scratch-mode 'lisp)

;; add music extensions if not already present...
(cl-loop for mode in '(("\\.clm$" . lisp-mode)
		    ("\\.cm$"  . lisp-mode)
		    ("\\.cmn$" . lisp-mode)
		    ("\\.ins$" . lisp-mode)
		    ("\\.fms$" . lisp-mode)
		    ("\\.asd$" . lisp-mode))
      do (add-to-list 'auto-mode-alist mode))

;; add music-related ignored file types...
(cl-loop for type in '(".midi" ".mid" ".snd" ".aiff" ".wav" ".osc"
		    ".fas" ".dfas" ".fasl" ".lib" ".ppcf" ".so"
		    ".dylib")
      do (add-to-list 'completion-ignored-extensions type))

;; set lisp buffers to slime mode...
(add-hook 'inferior-lisp-mode-hook
            (lambda ()
	      (slime-mode t)
	      (setq indent-tabs-mode nil)
	      ))

;; connect hook executes (cm) to set readtable etc and then removes
;; itself so that it doesnt interfere with other slime sessions.

(defun cm-start-hook ()
  ;;(slime-repl-send-string "(cm)")
  (slime-interactive-eval "(cm)")
  (remove-hook 'slime-connected-hook 'cm-start-hook)
  ;; aquamacs: hide inferior lisp buffer if visible after slime buffer
  ;; starts. This happens if user re-mouses original frame after doing
  ;; M-x cm before slime repl has been activated. if user then closes
  ;; the visible inferior-lisp frame the lisp session is hosed.
  ;; (when (member 'aquamacs features)
  ;;    (replace-buffer-in-windows (get-buffer "*inferior-lisp*")))
  (when (member 'aquamacs features)
    (let ((ilw (get-buffer-window " *inferior-lisp*" t)))
      (if ilw (delete-frame (window-frame ilw))))
    ))

;; Darwin: define COMMAND-E to evaluate expr a la MCL.
(if (equal system-type 'darwin)
    (global-set-key [(alt e)] 'slime-eval-expr))

;; add cm startup actions to inferior-lisp startup BEFORE repl has
;; been established

(defun cm-init-command (port coding)
  ;; get slime's inits 
  (let ((init (slime-init-command port coding)))
    ;; append system loading before repl bufer starts
    (dolist (s cm-systems)
      (setq init
	    (concat init (if (keywordp s)
			     (format "(use-system %s)\n" s)
			   (format "(use-system :%s)\n" s)))))
    init))

(defun load-and-start-cm ()
  (slime-eval '(ql:quickload "cm"))
  (slime-repl-send-string "(cm)")
  (switch-to-buffer (slime-repl-buffer)))

(defun load-and-start-cm-remove-hook ()
  "after starting cm remove this hook from slime-connected-hook"
  (load-and-start-cm)
  (setq slime-connected-hook
        (remove 'load-and-start-cm-remove-hook slime-connected-hook)))

(defun cm ()
  "Start CM"
  (interactive)
  (message "entering cm function.")
  (cond ((slime-connected-p)
         (load-and-start-cm))
	(t ;;; temporarily set slime-connected-hook to start cm
         (unless (member 'load-and-start-cm slime-connected-hook)
           (setq slime-connected-hook
                 (append slime-connected-hook '(load-and-start-cm-remove-hook))))
         (slime))))

(defun kill-cm ()
  "Kill *slime-repl* and all associated buffers."
  (interactive)
  (slime-repl-sayoonara))


(defun enable-cm-commands ()
  (interactive )
  ;; 1 stroke switching between repl and last editing buffer
  (global-set-key (kbd "<f8>") 'slime-toggle-repl)
  ;; eval before at or after point, region, or whole defun on whitespae
  (if (boundp 'slime-mode-map)
      (progn
        (define-key slime-mode-map (kbd "\C-x\C-e") 'slime-eval-last-expression)
        ;; indent line or region
        (define-key slime-mode-map (kbd "TAB") 'slime-indent-anything)
        ;; lookup cm function at point
        (define-key slime-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
        (define-key slime-repl-mode-map (kbd "\C-c\C-dc") 'cm-lookup)))
  (if (boundp 'sly-mode-map)
      (progn
        ;; (define-key lisp-mode-map (kbd "\C-x\C-e") 'sly-eval-last-expression)
        ;; ;; indent line or region
        ;; (define-key lisp-mode-map (kbd "TAB") 'slime-indent-anything)
        ;; lookup cm function at point
        (define-key lisp-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
        (define-key sly-mrepl-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
        ))
  )

(defun slime-toggle-repl ()
  "Toggle between *slime-repl* and last lisp or SAL buffer."
  (interactive)
  (if (slime-connected-p)
      (let ((repl (slime-repl-buffer)))
        (if repl
            (let ((this (current-buffer))
		  next)
              (if (eq repl this)
                  (setq next (cl-loop for b in (buffer-list)
				   when (with-current-buffer b
					  (or (eq major-mode 'lisp-mode)
					      (eq major-mode 'sal-mode)
					      ))
				   return b))
		(setq next (slime-repl-buffer)))
	      (when next
		;;(pop-to-buffer next)
		;;(switch-to-buffer-other-frame next)
		(switch-to-buffer next)))))))

(defun claim-scratch-buffer ()
  ;; if scratch buffer is empty set to slime or SAL mode
  (let ((scratch (get-buffer "*scratch*")))
    (if scratch
	(if (not (buffer-modified-p scratch))
	    (with-current-buffer scratch
	      (cond ((equal cm-scratch-mode 'lisp)
		     (lisp-mode)
		     (setq slime-buffer-package "cm")
		     (insert (format "(in-package :cm)\n\n"))
		     (goto-char (point-max)))
		    ((equal cm-scratch-mode 'sal)
		     (sal-mode)
		     (insert (format "; Use this buffer for SAL commands.\n\n"))
		     (goto-char (point-max)))
		    (t )))))))

(when (not (featurep 'xemacs))
  (defun region-exists-p ()
    (and mark-active ; simple.el
	 (not (null (mark))))))

(defun slime-eval-expr ()
  "Evals expr before point, at point, around point, whole region."
  (interactive)
  (if (region-exists-p )
      (slime-eval-region (region-beginning) (region-end))
    (let ((wspace '(?\  ?\t ?\r ?\n))
	  (left-char (char-before))
	  (right-char (char-after))
	  left-side right-side)
      (setq left-side
	    (if (or (not left-char)
		    (member left-char wspace)
		    (member left-char '(?\( )))
		(point)
	      (save-excursion
		(backward-sexp)
		(point))))
      (setq right-side
	    (if (or (not right-char)
		    (member right-char wspace)
		    (member right-char '(?\) ))
		    ;; dont look ahead if different sexp leftward
		    (and (< left-side (point))
			 (char-equal left-char ?\))))
		(point)
	      (save-excursion
		(forward-sexp)
		(point))))
      (if (equal left-side right-side)   
	  nil
	(slime-interactive-eval
	 (buffer-substring-no-properties left-side right-side))))))

(defun sly-eval-expr ()
  "Evals expr before point, at point, around point, whole region."
  (interactive)
  (if (region-exists-p )
      (sly-eval-region (region-beginning) (region-end))
    (let ((wspace '(?\  ?\t ?\r ?\n))
	  (left-char (char-before))
	  (right-char (char-after))
	  left-side right-side)
      (setq left-side
	    (if (or (not left-char)
		    (member left-char wspace)
		    (member left-char '(?\( )))
		(point)
	      (save-excursion
		(backward-sexp)
		(point))))
      (setq right-side
	    (if (or (not right-char)
		    (member right-char wspace)
		    (member right-char '(?\) ))
		    ;; dont look ahead if different sexp leftward
		    (and (< left-side (point))
			 (char-equal left-char ?\))))
		(point)
	      (save-excursion
		(forward-sexp)
		(point))))
      (if (equal left-side right-side)   
	  nil
	(sly-interactive-eval
	 (buffer-substring-no-properties left-side right-side))))))

(defun slime-indent-anything ()
  "Do line indentation/symbol completion; indent region if
selected; indent whole defun if prefixed."
  (interactive)
  (if current-prefix-arg
      (slime-reindent-defun )
    (if (and (region-exists-p)
	     (> (count-lines (region-beginning) (region-end)) 1))
	(lisp-indent-region (region-beginning) (region-end))
      (slime-indent-and-complete-symbol))))

;;;
;;; CM documentation hacks, mostly cribbed from hyperspec.
;;;

(defvar *common-music-doc-root* (concat "file://" (expand-file-name "~/quicklisp/local-projects/clamps/doc/"))
  "The root url for visiting CM documentation.")

(defun cm-doc (url)
  (interactive "FCM document:")
  (browse-url-firefox
   (concat *common-music-doc-root*
           url)
   nil))

(defun cm-lookup (entry)
  (interactive
   (list
    (let* ((it (thing-at-point 'symbol))
	   (sy (and it (downcase it))))
      (if (and sy (intern-soft sy *common-music-symbols*))
	  sy
	(completing-read "Lookup CM symbol: "
			 *common-music-symbols*
			 #'boundp t nil nil nil)))))
  (if entry
      (let ((sym (intern-soft (downcase entry) *common-music-symbols*)))
	(if (and sym (boundp sym))
	    (cm-doc (car (symbol-value sym)))))))

;;; (car (symbol-value (intern-soft "process" *common-music-symbols*)))

(defvar *common-music-doc-menu*
  `("Common Music"
    [ "Home Page" (cm-doc "cm.html")]
    [ "Installing" (cm-doc "install.html")]
    [ "Working in Emacs" (cm-doc "emacs.html")]
    "--"
    [ "Tutorials" (cm-doc "../etc/tutorials/")]
    [ "Examples"  (cm-doc "../etc/examples/")]
    "--"
    [ "Dictionary" (cm-doc "dict/index.html") ]
    [ "Lookup..." cm-lookup ]
    ))

;; add Common Music documentation menu to Slime...
;; last argument changed to make it workable under xemacs 21.4.19
;; Robert Matovinovi,c 20.07.2006, robert.matovinovic@web.de

;; (easy-menu-add-item menubar-slime
;; 		    '("Documentation")
;; 		    *common-music-doc-menu*
;; 		    ;(easy-menu-create-menu "Common Music" )
;; 		    )

(if (boundp 'sly-mode-map)
    (easy-menu-add-item sly-menu
		        '("Documentation")
		        *common-music-doc-menu*
                                        ;(easy-menu-create-menu "Common Music" )
		        ))

(defvar *common-music-symbols* (make-vector 63 0))

(mapcar
 (lambda (entry)
   (let ((symbol (intern (car entry)
			 *common-music-symbols*)))
     (if (boundp symbol)
	 (push (cadr entry) (symbol-value symbol))
       (set symbol (cdr entry)))))
 ;; *** generate by loading "/Lisp/cm/doc/dict/index.lisp"
 '(
; warning: /Lisp/cm/doc/cm-dict/index.html#osc-stream-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/cm-dict/index.html#sc-file-cls.html has no <title> tag.
; warning: /Lisp/cm/doc/cm-dict/index.html#sc-stream-cls.html has no <title> tag.
   ("*beat*" "cm-dict/index.html#beat-var.html")
   ("*chromatic-scale*" "cm-dict/index.html#chromatic-scale-var.html")
   ("*loudest*" "cm-dict/index.html#loudest-var.html")
   ("*midi-channel-map*" "cm-dict/index.html#midi-channel-map-var.html")
   ("*midi-connections*" "cm-dict/index.html#midi-connections-var.html")
   ("*portmidi-default-filter*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-filter*")
   ("*portmidi-default-inbuf-size*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-inbuf-size*")
   ("*portmidi-default-input*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-input*")
   ("*portmidi-default-latency*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-latency*")
   ("*portmidi-default-mask*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-mask*")
   ("*portmidi-default-outbuf-size*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-outbuf-size*")
   ("*portmidi-default-output*" "cm-dict/index.html#portmidi-topic.html#*portmidi-default-output*")
   ("*power*" "cm-dict/index.html#power-var.html")
   ("*scale*" "cm-dict/index.html#scale-var.html")
   ("*softest*" "cm-dict/index.html#softest-var.html")
   ("*tempo*" "cm-dict/index.html#tempo-var.html")
   ("accumulation" "cm-dict/index.html#accumulation-cls.html")
   ("active-sensing-p" "cm-dict/index.html#midi-topic.html#active-sensing-p")
   ("active-sensing-route" "cm-dict/index.html#midi-topic.html#active-sensing-route")
   ("amplitude" "cm-dict/index.html#amplitude-fn.html")
   ("append-object" "cm-dict/index.html#append-object-fn.html")
   ("audio-file" "cm-dict/index.html#audio-file-cls.html")
   ("axis" "cm-dict/index.html#axis-fn.html")
   ("axis" "cm-dict/index.html#axis-cls.html")
   ("between" "cm-dict/index.html#between-fn.html")
   ("cable-select-cable" "cm-dict/index.html#midi-topic.html#cable-select-cable")
   ("cable-select-p" "cm-dict/index.html#midi-topic.html#cable-select-p")
   ("cable-select-route" "cm-dict/index.html#midi-topic.html#cable-select-route")
   ("cd" "cm-dict/index.html#cd-fn.html")
   ("cents->scaler" "cm-dict/index.html#cents-gtscaler-fn.html")
   ("channel-message-channel" "cm-dict/index.html#midi-topic.html#channel-message-channel")
   ("channel-message-data1" "cm-dict/index.html#midi-topic.html#channel-message-data1")
   ("channel-message-data2" "cm-dict/index.html#midi-topic.html#channel-message-data2")
   ("channel-message-opcode" "cm-dict/index.html#midi-topic.html#channel-message-opcode")
   ("channel-message-p" "cm-dict/index.html#midi-topic.html#channel-message-p")
   ("channel-pressure-channel" "cm-dict/index.html#midi-topic.html#channel-pressure-channel")
   ("channel-pressure-p" "cm-dict/index.html#midi-topic.html#channel-pressure-p")
   ("channel-pressure-pressure" "cm-dict/index.html#midi-topic.html#channel-pressure-pressure")
   ("chord" "cm-dict/index.html#chord-cls.html")
   ("clm-file" "cm-dict/index.html#clm-file-cls.html")
   ("cm-version" "cm-dict/index.html#cm-version-fn.html")
   ("cm.sh" "cm-dict/index.html#cm-sh.html")
   ("cmio" "cm-dict/index.html#cmio-fn.html")
   ("cmn" "cm-dict/index.html#cmn-cls.html")
   ("cmn-file" "cm-dict/index.html#cmn-file-cls.html")
   ("continue-p" "cm-dict/index.html#midi-topic.html#continue-p")
   ("continue-route" "cm-dict/index.html#midi-topic.html#continue-route")
   ("control-change-channel" "cm-dict/index.html#midi-topic.html#control-change-channel")
   ("control-change-controller" "cm-dict/index.html#midi-topic.html#control-change-controller")
   ("control-change-p" "cm-dict/index.html#midi-topic.html#control-change-p")
   ("control-change-value" "cm-dict/index.html#midi-topic.html#control-change-value")
   ("copier" "cm-dict/index.html#copier-cls.html")
   ("copy-object" "cm-dict/index.html#copy-object-fn.html")
   ("copyright-note-p" "cm-dict/index.html#midi-topic.html#copyright-note-p")
   ("cue-point-p" "cm-dict/index.html#midi-topic.html#cue-point-p")
   ("cycle" "cm-dict/index.html#cycle-cls.html")
   ("date-and-time" "cm-dict/index.html#date-and-time-fn.html")
   ("decimals" "cm-dict/index.html#decimals-fn.html")
   ("decode-interval" "cm-dict/index.html#decode-interval-fn.html")
   ("defaxis" "cm-dict/index.html#defaxis-mac.html")
   ("defobject" "cm-dict/index.html#defobject-mac.html")
   ("doeach" "cm-dict/index.html#doeach-mac.html")
   ("drunk" "cm-dict/index.html#drunk-fn.html")
   ("dumposc" "cm-dict/index.html#dumposc-fn.html")
   ("eod?" "cm-dict/index.html#eodqmk-fn.html")
   ("eop?" "cm-dict/index.html#eopqmk-fn.html")
   ("eot-p" "cm-dict/index.html#midi-topic.html#eot-p")
   ("eox-p" "cm-dict/index.html#midi-topic.html#eox-p")
   ("eox-route" "cm-dict/index.html#midi-topic.html#eox-route")
   ("events" "cm-dict/index.html#events-fn.html")
   ("expl" "cm-dict/index.html#expl-fn.html")
   ("explseg" "cm-dict/index.html#explseg-fn.html")
   ("explsegs" "cm-dict/index.html#explsegs-fn.html")
   ("f" "cm-dict/index.html#f-cls.html")
   ("false" "cm-dict/index.html#false-var.html")
   ("find-object" "cm-dict/index.html#find-object-fn.html")
   ("fit" "cm-dict/index.html#fit-fn.html")
   ("fm-spectrum" "cm-dict/index.html#fm-spectrum-fn.html")
   ("fold-objects" "cm-dict/index.html#fold-objects-fn.html")
   ("fomus-file" "cm-dict/index.html#fomus-file-cls.html")
   ("graph" "cm-dict/index.html#graph-cls.html")
   ("harmonics" "cm-dict/index.html#harmonics-fn.html")
   ("heap" "cm-dict/index.html#heap-cls.html")
   ("hertz" "cm-dict/index.html#hertz-fn.html")
   ("histogram" "cm-dict/index.html#histogram-fn.html")
   ("i" "cm-dict/index.html#i-cls.html")
   ("import-events" "cm-dict/index.html#import-events-fn.html")
   ("input" "cm-dict/index.html#input-fn.html")
   ("insert-object" "cm-dict/index.html#insert-object-fn.html")
   ("instrument-name-p" "cm-dict/index.html#midi-topic.html#instrument-name-p")
   ("interp" "cm-dict/index.html#interp-fn.html")
   ("interpl" "cm-dict/index.html#interpl-fn.html")
   ("interval" "cm-dict/index.html#interval-fn.html")
   ("invert" "cm-dict/index.html#invert-fn.html")
   ("io" "cm-dict/index.html#io-mac.html")
   ("join" "cm-dict/index.html#join-cls.html")
   ("key-pressure-channel" "cm-dict/index.html#midi-topic.html#key-pressure-channel")
   ("key-pressure-key" "cm-dict/index.html#midi-topic.html#key-pressure-key")
   ("key-pressure-p" "cm-dict/index.html#midi-topic.html#key-pressure-p")
   ("key-pressure-pressure" "cm-dict/index.html#midi-topic.html#key-pressure-pressure")
   ("key-signature-p" "cm-dict/index.html#midi-topic.html#key-signature-p")
   ("keynum" "cm-dict/index.html#keynum-fn.html")
   ("line" "cm-dict/index.html#line-cls.html")
   ("list-named-objects" "cm-dict/index.html#list-named-objects-fn.html")
   ("list-objects" "cm-dict/index.html#list-objects-fn.html")
   ("log-axis" "cm-dict/index.html#log-axis-cls.html")
   ("lookup" "cm-dict/index.html#lookup-fn.html")
   ("lyric-p" "cm-dict/index.html#midi-topic.html#lyric-p")
   ("make-active-sensing" "cm-dict/index.html#midi-topic.html#make-active-sensing")
   ("make-cable-select" "cm-dict/index.html#midi-topic.html#make-cable-select")
   ("make-channel-message" "cm-dict/index.html#midi-topic.html#make-channel-message")
   ("make-channel-pressure" "cm-dict/index.html#midi-topic.html#make-channel-pressure")
   ("make-cm" "cm-dict/index.html#make-cm-fn.html")
   ("make-continue" "cm-dict/index.html#midi-topic.html#make-continue")
   ("make-control-change" "cm-dict/index.html#midi-topic.html#make-control-change")
   ("make-copyright-note" "cm-dict/index.html#midi-topic.html#make-copyright-note")
   ("make-cue-point" "cm-dict/index.html#midi-topic.html#make-cue-point")
   ("make-eot" "cm-dict/index.html#midi-topic.html#make-eot")
   ("make-eox" "cm-dict/index.html#midi-topic.html#make-eox")
   ("make-instrument-name" "cm-dict/index.html#midi-topic.html#make-instrument-name")
   ("make-key-pressure" "cm-dict/index.html#midi-topic.html#make-key-pressure")
   ("make-key-signature" "cm-dict/index.html#midi-topic.html#make-key-signature")
   ("make-lyric" "cm-dict/index.html#midi-topic.html#make-lyric")
   ("make-marker" "cm-dict/index.html#midi-topic.html#make-marker")
   ("make-meta-message" "cm-dict/index.html#midi-topic.html#make-meta-message")
   ("make-midi-channel" "cm-dict/index.html#midi-topic.html#make-midi-channel")
   ("make-midi-port" "cm-dict/index.html#midi-topic.html#make-midi-port")
   ("make-mtc-quarter-frame" "cm-dict/index.html#midi-topic.html#make-mtc-quarter-frame")
   ("make-note-off" "cm-dict/index.html#midi-topic.html#make-note-off")
   ("make-note-on" "cm-dict/index.html#midi-topic.html#make-note-on")
   ("make-pitch-bend" "cm-dict/index.html#midi-topic.html#make-pitch-bend")
   ("make-program-change" "cm-dict/index.html#midi-topic.html#make-program-change")
   ("make-sequence-number" "cm-dict/index.html#midi-topic.html#make-sequence-number")
   ("make-sequencer-event" "cm-dict/index.html#midi-topic.html#make-sequencer-event")
   ("make-sequence_track-name" "cm-dict/index.html#midi-topic.html#make-sequence_track-name")
   ("make-smpte-offset" "cm-dict/index.html#midi-topic.html#make-smpte-offset")
   ("make-song-position" "cm-dict/index.html#midi-topic.html#make-song-position")
   ("make-song-select" "cm-dict/index.html#midi-topic.html#make-song-select")
   ("make-start" "cm-dict/index.html#midi-topic.html#make-start")
   ("make-stop" "cm-dict/index.html#midi-topic.html#make-stop")
   ("make-sysex" "cm-dict/index.html#midi-topic.html#make-sysex")
   ("make-system-message" "cm-dict/index.html#midi-topic.html#make-system-message")
   ("make-system-reset" "cm-dict/index.html#midi-topic.html#make-system-reset")
   ("make-tempo-change" "cm-dict/index.html#midi-topic.html#make-tempo-change")
   ("make-text-event" "cm-dict/index.html#midi-topic.html#make-text-event")
   ("make-time-signature" "cm-dict/index.html#midi-topic.html#make-time-signature")
   ("make-timing-clock" "cm-dict/index.html#midi-topic.html#make-timing-clock")
   ("make-timing-tick" "cm-dict/index.html#midi-topic.html#make-timing-tick")
   ("make-tune-request" "cm-dict/index.html#midi-topic.html#make-tune-request")
   ("map-objects" "cm-dict/index.html#map-objects-fn.html")
   ("map-pattern-data" "cm-dict/index.html#map-pattern-data-fn.html")
   ("map-subcontainers" "cm-dict/index.html#map-subcontainers-fn.html")
   ("map-subobjects" "cm-dict/index.html#map-subobjects-fn.html")
   ("marker-p" "cm-dict/index.html#midi-topic.html#marker-p")
   ("markov" "cm-dict/index.html#markov-cls.html")
   ("markov-analyze" "cm-dict/index.html#markov-analyze-fn.html")
   ("meta-message-p" "cm-dict/index.html#midi-topic.html#meta-message-p")
   ("meta-message-type" "cm-dict/index.html#midi-topic.html#meta-message-type")
   ("midi" "cm-dict/index.html#midi-cls.html")
   ("midi-chan-event" "cm-dict/index.html#midi-chan-event-cls.html")
   ("midi-channel-p" "cm-dict/index.html#midi-topic.html#midi-channel-p")
   ("midi-channel-pressure" "cm-dict/index.html#midi-channel-pressure-cls.html")
   ("midi-control-change" "cm-dict/index.html#midi-control-change-cls.html")
   ("midi-copy-message" "cm-dict/index.html#midi-topic.html#midi-copy-message")
   ("midi-eot" "cm-dict/index.html#midi-eot-cls.html")
   ("midi-file" "cm-dict/index.html#midi-file-cls.html")
   ("midi-file-print" "cm-dict/index.html#midi-file-print-fn.html")
   ("midi-key-pressure" "cm-dict/index.html#midi-key-pressure-cls.html")
   ("midi-key-signature" "cm-dict/index.html#midi-key-signature-cls.html")
   ("midi-note-off" "cm-dict/index.html#midi-note-off-cls.html")
   ("midi-note-on" "cm-dict/index.html#midi-note-on-cls.html")
   ("midi-pitch-bend" "cm-dict/index.html#midi-pitch-bend-cls.html")
   ("midi-port-event" "cm-dict/index.html#midi-port-event-cls.html")
   ("midi-port-p" "cm-dict/index.html#midi-topic.html#midi-port-p")
   ("midi-print-message" "cm-dict/index.html#midi-topic.html#midi-print-message")
   ("midi-program-change" "cm-dict/index.html#midi-program-change-cls.html")
   ("midi-sequence-number" "cm-dict/index.html#midi-sequence-number-cls.html")
   ("midi-sequencer-event" "cm-dict/index.html#midi-sequencer-event-cls.html")
   ("midi-smpte-offset" "cm-dict/index.html#midi-smpte-offset-cls.html")
   ("midi-stream" "cm-dict/index.html#midi-stream-cls.html")
   ("midi-system-event" "cm-dict/index.html#midi-system-event-cls.html")
   ("midi-tempo-change" "cm-dict/index.html#midi-tempo-change-cls.html")
   ("midi-text-event" "cm-dict/index.html#midi-text-event-cls.html")
   ("midi-time-signature" "cm-dict/index.html#midi-time-signature-cls.html")
   ("midishare-open" "cm-dict/index.html#midishare-topic.html#midishare-open")
   ("midishare-open?" "cm-dict/index.html#midishare-topic.html#midishare-open?")
   ("mode" "cm-dict/index.html#mode-cls.html")
   ("ms:midiprintev" "cm-dict/index.html#midishare-topic.html#ms:midiprintev")
   ("ms:new" "cm-dict/index.html#midishare-topic.html#ms:new")
   ("mtc-quarter-frame-nibble" "cm-dict/index.html#midi-topic.html#mtc-quarter-frame-nibble")
   ("mtc-quarter-frame-p" "cm-dict/index.html#midi-topic.html#mtc-quarter-frame-p")
   ("mtc-quarter-frame-route" "cm-dict/index.html#midi-topic.html#mtc-quarter-frame-route")
   ("mtc-quarter-frame-tag" "cm-dict/index.html#midi-topic.html#mtc-quarter-frame-tag")
   ("new" "cm-dict/index.html#new-mac.html")
   ("next" "cm-dict/index.html#next-fn.html")
   ("note" "cm-dict/index.html#note-fn.html")
   ("note-accidental" "cm-dict/index.html#note-accidental-fn.html")
   ("note-name" "cm-dict/index.html#note-name-fn.html")
   ("note-off-channel" "cm-dict/index.html#midi-topic.html#note-off-channel")
   ("note-off-key" "cm-dict/index.html#midi-topic.html#note-off-key")
   ("note-off-p" "cm-dict/index.html#midi-topic.html#note-off-p")
   ("note-off-velocity" "cm-dict/index.html#midi-topic.html#note-off-velocity")
   ("note-on-channel" "cm-dict/index.html#midi-topic.html#note-on-channel")
   ("note-on-key" "cm-dict/index.html#midi-topic.html#note-on-key")
   ("note-on-p" "cm-dict/index.html#midi-topic.html#note-on-p")
   ("note-on-velocity" "cm-dict/index.html#midi-topic.html#note-on-velocity")
   ("now" "cm-dict/index.html#now-fn.html")
   ("object->cmn" "cm-dict/index.html#object-gtcmn-fn.html")
   ("object-name" "cm-dict/index.html#object-name-fn.html")
   ("object-parameters" "cm-dict/index.html#object-parameters-fn.html")
   ("object-time" "cm-dict/index.html#object-time-fn.html")
   ("octave-number" "cm-dict/index.html#octave-number-fn.html")
   ("odds" "cm-dict/index.html#odds-fn.html")
   ("output" "cm-dict/index.html#output-fn.html")
   ("palindrome" "cm-dict/index.html#palindrome-cls.html")
   ("pattern-state" "cm-dict/index.html#pattern-state-fn.html")
   ("pattern-value" "cm-dict/index.html#pattern-value-fn.html")
   ("pattern?" "cm-dict/index.html#patternqmk-fn.html")
   ("pick" "cm-dict/index.html#pick-fn.html")
   ("pickl" "cm-dict/index.html#pickl-fn.html")
   ("pitch-bend-channel" "cm-dict/index.html#midi-topic.html#pitch-bend-channel")
   ("pitch-bend-lsb" "cm-dict/index.html#midi-topic.html#pitch-bend-lsb")
   ("pitch-bend-msb" "cm-dict/index.html#midi-topic.html#pitch-bend-msb")
   ("pitch-bend-p" "cm-dict/index.html#midi-topic.html#pitch-bend-p")
   ("pitch-class" "cm-dict/index.html#pitch-class-fn.html")
   ("play" "cm-dict/index.html#play-fn.html")
   ("plotter" "cm-dict/index.html#plotter-fn.html")
   ("plotter-add-layer" "cm-dict/index.html#plotter-add-layer-fn.html")
   ("plotter-data" "cm-dict/index.html#plotter-data-fn.html")
   ("plotter-front-styling" "cm-dict/index.html#plotter-front-styling-fn.html")
   ("plotter-property" "cm-dict/index.html#plotter-property-fn.html")
   ("plotter-redraw" "cm-dict/index.html#plotter-redraw-fn.html")
   ("plotter-redraw" "cm-dict/index.html#plotter-close-fn.html")
   ("plotter-scroll" "cm-dict/index.html#plotter-scroll-fn.html")
   ("plotter-zoom" "cm-dict/index.html#plotter-zoom-fn.html")
   ("pm:countdevices" "cm-dict/index.html#portmidi-topic.html#pm:countdevices")
   ("pm:getdefaultinputdeviceid" "cm-dict/index.html#portmidi-topic.html#pm:getdefaultinputdeviceid")
   ("pm:getdefaultoutputdeviceid" "cm-dict/index.html#portmidi-topic.html#pm:getdefaultoutputdeviceid")
   ("pm:getdeviceinfo" "cm-dict/index.html#portmidi-topic.html#pm:getdeviceinfo")
   ("pm:time" "cm-dict/index.html#portmidi-topic.html#pm:time")
   ("point" "cm-dict/index.html#point-cls.html")
   ("portmidi-close" "cm-dict/index.html#portmidi-topic.html#portmidi-close")
   ("portmidi-open" "cm-dict/index.html#portmidi-topic.html#portmidi-open")
   ("portmidi-open?" "cm-dict/index.html#portmidi-topic.html#portmidi-open?")
   ("portmidi-record!" "cm-dict/index.html#portmidi-topic.html#portmidi-record")
   ("portmidi-stream" "cm-dict/index.html#portmidi-topic.html#portmidi-stream")
   ("prime-form" "cm-dict/index.html#prime-form-fn.html")
   ("process" "cm-dict/index.html#process-mac.html")
   ("program-change-channel" "cm-dict/index.html#midi-topic.html#program-change-channel")
   ("program-change-p" "cm-dict/index.html#midi-topic.html#program-change-p")
   ("program-change-program" "cm-dict/index.html#midi-topic.html#program-change-program")
   ("pval" "cm-dict/index.html#pval-mac.html")
   ("pval" "cm-dict/index.html#pval-cls.html")
   ("pwd" "cm-dict/index.html#pwd-fn.html")
   ("quantize" "cm-dict/index.html#quantize-fn.html")
   ("ran" "cm-dict/index.html#ran-fn.html")
   ("range" "cm-dict/index.html#range-cls.html")
   ("ransegs" "cm-dict/index.html#ransegs-fn.html")
   ("receive" "cm-dict/index.html#receive-fn.html")
   ("receiver?" "cm-dict/index.html#receiverqmk-fn.html")
   ("remove-object" "cm-dict/index.html#remove-object-fn.html")
   ("remove-receiver!" "cm-dict/index.html#remove-receiver-fn.html")
   ("remove-subobjects" "cm-dict/index.html#remove-subobjects-fn.html")
   ("rescale" "cm-dict/index.html#rescale-fn.html")
   ("rescale-envelope" "cm-dict/index.html#rescale-envelope-fn.html")
   ("rewrite" "cm-dict/index.html#rewrite-cls.html")
   ("rewrite-generation" "cm-dict/index.html#rewrite-generation-fn.html")
   ("rhythm" "cm-dict/index.html#rhythm-fn.html")
   ("rm-spectrum" "cm-dict/index.html#rm-spectrum-fn.html")
   ("rotation" "cm-dict/index.html#rotation-cls.html")
;;; (rewritten in clamps)   ("rts" "cm-dict/index.html#rts-fn.html")
   ("rts-continue" "cm-dict/index.html#rts-continue-fn.html")
   ("rts-pause" "cm-dict/index.html#rts-pause-fn.html")
   ("rts-stop" "cm-dict/index.html#rts-stop-fn.html")
   ("rts?" "cm-dict/index.html#rtsqmk-fn.html")
   ("save-object" "cm-dict/index.html#save-object-fn.html")
   ("sc-clearsched" "cm-dict/index.html#supercollider-topic.html#sc-clearsched")
   ("sc-close" "cm-dict/index.html#supercollider-topic.html#sc-close")
   ("sc-dumposc" "cm-dict/index.html#supercollider-topic.html#sc-dumposc")
   ("sc-flush" "cm-dict/index.html#supercollider-topic.html#sc-flush")
   ("sc-notify" "cm-dict/index.html#supercollider-topic.html#sc-notify")
   ("sc-open" "cm-dict/index.html#supercollider-topic.html#sc-open")
   ("sc-open?" "cm-dict/index.html#supercollider-topic.html#sc-open?")
   ("sc-quit" "cm-dict/index.html#supercollider-topic.html#sc-quit")
   ("scale-max" "cm-dict/index.html#scale-max-fn.html")
   ("scale-min" "cm-dict/index.html#scale-min-fn.html")
   ("scale-mod" "cm-dict/index.html#scale-mod-fn.html")
   ("scale-order" "cm-dict/index.html#scale-order-fn.html")
   ("scale<" "cm-dict/index.html#scalelt-fn.html")
   ("scale<=" "cm-dict/index.html#scalelteql-fn.html")
   ("scale=" "cm-dict/index.html#scaleeql-fn.html")
   ("scale>" "cm-dict/index.html#scalegt-fn.html")
   ("scale>=" "cm-dict/index.html#scalegteql-fn.html")
   ("scaler->cents" "cm-dict/index.html#scaler-gtcents-fn.html")
   ("sco-file" "cm-dict/index.html#sco-file-cls.html")
   ("seq" "cm-dict/index.html#seq-cls.html")
   ("sequence-number-p" "cm-dict/index.html#midi-topic.html#sequence-number-p")
   ("sequencer-event-p" "cm-dict/index.html#midi-topic.html#sequencer-event-p")
   ("sequence_track-name-p" "cm-dict/index.html#midi-topic.html#sequence_track-name-p")
   ("set-clm-output-hook!" "cm-dict/index.html#set-clm-output-hook-fn.html")
   ("set-midi-output-hook!" "cm-dict/index.html#set-midi-output-hook-fn.html")
   ("set-receiver!" "cm-dict/index.html#set-receiver-fn.html")
   ("set-sco-output-hook!" "cm-dict/index.html#set-sco-output-hook-fn.html")
   ("shell" "cm-dict/index.html#shell-fn.html")
   ("shuffle" "cm-dict/index.html#shuffle-fn.html")
   ("smpte-offset-p" "cm-dict/index.html#midi-topic.html#smpte-offset-p")
   ("song-position-lsb" "cm-dict/index.html#midi-topic.html#song-position-lsb")
   ("song-position-msb" "cm-dict/index.html#midi-topic.html#song-position-msb")
   ("song-position-p" "cm-dict/index.html#midi-topic.html#song-position-p")
   ("song-position-route" "cm-dict/index.html#midi-topic.html#song-position-route")
   ("song-select-p" "cm-dict/index.html#midi-topic.html#song-select-p")
   ("song-select-route" "cm-dict/index.html#midi-topic.html#song-select-route")
   ("song-select-song" "cm-dict/index.html#midi-topic.html#song-select-song")
   ("sprout" "cm-dict/index.html#sprout-fn.html")
   ("start-p" "cm-dict/index.html#midi-topic.html#start-p")
   ("start-route" "cm-dict/index.html#midi-topic.html#start-route")
   ("stop" "cm-dict/index.html#stop-fn.html")
   ("stop-p" "cm-dict/index.html#midi-topic.html#stop-p")
   ("stop-route" "cm-dict/index.html#midi-topic.html#stop-route")
   ("subcontainers" "cm-dict/index.html#subcontainers-fn.html")
   ("subobjects" "cm-dict/index.html#subobjects-fn.html")
   ("sv" "cm-dict/index.html#sv-mac.html")
   ("sv*" "cm-dict/index.html#svstar-mac.html")
   ("sv+" "cm-dict/index.html#svplus-mac.html")
   ("sysex-p" "cm-dict/index.html#midi-topic.html#sysex-p")
   ("sysex-route" "cm-dict/index.html#midi-topic.html#sysex-route")
   ("system-message-data1" "cm-dict/index.html#midi-topic.html#system-message-data1")
   ("system-message-data2" "cm-dict/index.html#midi-topic.html#system-message-data2")
   ("system-message-p" "cm-dict/index.html#midi-topic.html#system-message-p")
   ("system-message-route" "cm-dict/index.html#midi-topic.html#system-message-route")
   ("system-message-status" "cm-dict/index.html#midi-topic.html#system-message-status")
   ("system-reset-p" "cm-dict/index.html#midi-topic.html#system-reset-p")
   ("system-reset-route" "cm-dict/index.html#midi-topic.html#system-reset-route")
   ("tempo-change-p" "cm-dict/index.html#midi-topic.html#tempo-change-p")
   ("tendency" "cm-dict/index.html#tendency-fn.html")
   ("text-event-p" "cm-dict/index.html#midi-topic.html#text-event-p")
   ("thunk" "cm-dict/index.html#thunk-cls.html")
   ("time-signature-p" "cm-dict/index.html#midi-topic.html#time-signature-p")
   ("timing-clock-p" "cm-dict/index.html#midi-topic.html#timing-clock-p")
   ("timing-clock-route" "cm-dict/index.html#midi-topic.html#timing-clock-route")
   ("timing-tick-p" "cm-dict/index.html#midi-topic.html#timing-tick-p")
   ("timing-tick-route" "cm-dict/index.html#midi-topic.html#timing-tick-route")
   ("transpose" "cm-dict/index.html#transpose-fn.html")
   ("transposer" "cm-dict/index.html#transposer-cls.html")
   ("true" "cm-dict/index.html#true-var.html")
   ("tune-request-p" "cm-dict/index.html#midi-topic.html#tune-request-p")
   ("tune-request-route" "cm-dict/index.html#midi-topic.html#tune-request-route")
   ("tuning" "cm-dict/index.html#tuning-cls.html")
   ("use-system" "cm-dict/index.html#use-system-fn.html")
   ("vary" "cm-dict/index.html#vary-fn.html")
   ("wait" "cm-dict/index.html#wait-fn.html")
   ("wait-until" "cm-dict/index.html#wait-until-fn.html")
   ("weighting" "cm-dict/index.html#weighting-cls.html"))
 )

(setf cm-font-lock-keywords
      (cons '("\\<\\(process\\|cycle\\|object\\|...\\)\\>"
	      . font-lock-reference-face)
            lisp-font-lock-keywords)
      lisp-font-lock-keywords cm-font-lock-keywords)

;; eof
(enable-cm-commands)
(provide 'cm)
