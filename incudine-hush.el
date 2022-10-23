(defun incudine-hush ()
  (interactive)
  (progn
    (slime-interactive-eval "(incudine:flush-pending)")
    (slime-interactive-eval "(dotimes (chan 16) (cm::sprout (cm::new cm::midi-control-change :time 0 :controller 123 :value 127 :channel chan)))")
;;;    (slime-interactive-eval "(scratch::node-free-unprotected)")
    (slime-interactive-eval "(scratch::node-free-all)")
;;;    (slime-interactive-eval "(uptothree02::osc-stop)")
;;;    (slime-interactive-eval "(uptothree02::osc-notes-off)")
    ))

(defun incudine-rt-start ()
  (interactive)
  (slime-interactive-eval "(incudine:rt-start)"))

(defun incudine-rt-stop ()
  (interactive)
  (slime-interactive-eval "(incudine:rt-stop)"))

(defun test-midi ()
  (interactive)
  (slime-interactive-eval "(cm::testmidi)"))

(define-key lisp-mode-map (kbd "C-.") 'incudine-hush)
(define-key lisp-mode-map (kbd "C-c C-.") 'incudine-rt-stop)
(define-key lisp-mode-map (kbd "C-c M-.") 'incudine-rt-start)
(define-key lisp-mode-map (kbd "C-c t") 'test-midi)
