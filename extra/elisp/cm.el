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
  (if (boundp 'sly-mrepl-mode-map)
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

(load "cm-dict.el")

(setf cm-font-lock-keywords
      (cons '("\\<\\(process\\|cycle\\|object\\|...\\)\\>"
	      . font-lock-reference-face)
            lisp-font-lock-keywords)
      lisp-font-lock-keywords cm-font-lock-keywords)

(add-hook
 'sly-mrepl-mode-hook
 (lambda ()
   (enable-cm-commands)))

(provide 'cm)
;; eof
