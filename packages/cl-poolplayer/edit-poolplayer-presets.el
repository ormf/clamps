(defun edit-poolplayer-preset (str ref)
  (set-buffer (find-file-noselect poolplayer-preset-file))
  (delete-region (point-min) (point-max))
  (insert "(in-package :cl-poolplayer)\n\n;;; preset: ")
  (insert (format "%s\n\n" ref))
  (insert (replace-regexp-in-string "cl-poolplayer::" ""
                                    (replace-regexp-in-string "orm-utils:" "" str)))
;;;  (insert (format "\n\n(state-store-curr-preset %s)" ref))
  (insert "\n\n;;; (save-route-presets)")
  (delete-region (point) (point-max))
  (goto-char 34)
  (forward-line)
  (forward-line)
  (sly-reindent-defun)
  (forward-line)
  (forward-line)
  (forward-line)
  (forward-line)
  (set-buffer-modified-p nil))

(defun next-poolplayer-preset ()
    (interactive)
    (sly-mrepl-send-string "(cl-poolplayer::next-poolplayer-preset)"))

(defun previous-poolplayer-preset ()
    (interactive)
    (sly-mrepl-send-string "(cl-poolplayer::previous-poolplayer-preset)")
    (save-excursion
      (switch-to-buffer (get-buffer "curr-preset.lisp"))))

(define-key lisp-mode-map (kbd "M-<left>") 'previous-poolplayer-preset)
(define-key lisp-mode-map (kbd "M-<right>") 'next-poolplayer-preset)

(save-excursion
  (switch-to-buffer (get-buffer "curr-preset.lisp")))
