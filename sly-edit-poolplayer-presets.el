(setq poolplayer-preset-file "/home/orm/work/programmieren/lisp/cl-poolplayer/curr-preset.lisp")

(defun edit-poolplayer-preset (str ref)
  (set-buffer (find-file-noselect orgel-preset-file))
  (delete-region (point-min) (point-max))
  (insert "(in-package :cl-poolplayer)\n\n;;; preset: ")
  (insert (format "%s\n\n" ref))
  (insert (replace-regexp-in-string "cl-poolplayer::" ""
                                    (replace-regexp-in-string "orm-utils:" "" str)))
;;;  (insert (format "\n\n(state-store-curr-preset %s)" ref))
  (insert (format "\n\n;;; (show-poolplayer-preset %s)\n;;; (save-poolplayer-presets)" ref))
  (delete-region (point) (point-max))
  (goto-char 0)
  (forward-line)
  (forward-line)
  (forward-line)
  (forward-line)
  (indent-sexp)
  (forward-line)
  (forward-line)
  (forward-char)
  (forward-char)
  (save-buffer))

(defun next-poolplayer-preset ()
    (interactive)
    (sly-interactive-eval "(cl-poolplayer::next-poolplayer-preset)")
    (save-excursion
      (switch-to-buffer (get-buffer "curr-preset.lisp"))))

(defun previous-poolplayer-preset ()
    (interactive)
    (sly-interactive-eval "(cl-poolplayer::previous-poolplayer-preset)")
    (save-excursion
      (switch-to-buffer (get-buffer "curr-preset.lisp"))))

(define-key lisp-mode-map (kbd "M-<left>") 'previous-poolplayer-preset)
(define-key lisp-mode-map (kbd "M-<right>") 'next-poolplayer-preset)

(save-excursion
  (switch-to-buffer (get-buffer "curr-preset.lisp")))
