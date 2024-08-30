;;; call with:
;;; emacs -q --eval "(defvar *clamps-doc-dir* \"`pwd`\")" --batch --load generate-html.el
;;; (defvar *clamps-doc-dir*)
;;; (setq *clamps-doc-dir* "/home/orm/work/programmieren/lisp/clamps/doc")
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/"))
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/htmlize-20240527.1456"))
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/rainbow-delimiters-20210515.1254"))

(load (format "%s%s" *clamps-doc-dir* "/../extra/elisp/font-lock-add-ons.el"))

(require 'org)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'htmlize)
(require 'font-lock)
(require 'htmlize-autoloads)
(require 'rainbow-delimiters)

(setq org-html-htmlize-output-type 'css)

;; (custom-set-faces
;;  '(font-lock-builtin-face ((t (:foreground "aquamarine"))))
;;  '(font-lock-comment-face ((t (:foreground "light blue"))))
;;  '(font-lock-constant-face ((t (:foreground "pale green"))))
;;  '(font-lock-doc-face ((t (:foreground "light sky blue"))))
;;  '(font-lock-doc-string-face ((t (:foreground "sky blue"))))
;;  '(font-lock-function-name-face ((t (:bold t :foreground "aquamarine" :weight bold))))
;;  '(font-lock-keyword-face ((t (:bold t :foreground "pale turquoise" :weight bold))))
;;  '(font-lock-reference-face ((t (:foreground "pale green"))))
;;  '(font-lock-string-face ((t (:foreground "light sky blue"))))
;;  '(font-lock-type-face ((t (:bold t :foreground "sky blue" :weight bold))))
;;  '(font-lock-variable-name-face ((t (:bold t :foreground "turquoise" :weight bold))))
;;  '(font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "dark gray"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "deep pink"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "green yellow"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow1"))))
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "gold3"))))
;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "orange3"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "DarkOrange4")))))

(add-hook
 'lisp-mode-hook
 (lambda ()
   (rainbow-delimiters-mode t)
   (font-lock-mode 1)
   (setq font-lock-mode 1)))

(global-font-lock-mode 1)
(setq rainbow-delimiters-mode t)
(setq font-lock-mode 1)

(load (format "%s%s" *clamps-doc-dir*
              "/../extra/elisp/ox.el"))

(load (format "%s%s" *clamps-doc-dir*
              "/../extra/elisp/ox-html.el"))

(load (format "%s%s" *clamps-doc-dir*
              "/../extra/elisp/clamps-links.el"))

(setq org-confirm-babel-evaluate nil)
;;;(setq org-export-with-broken-links t)

(defun generate-html (org-file)
    (find-file org-file)
    (org-html-export-to-multipage))

(generate-html
 (format "%s%s" *clamps-doc-dir*
         "/overview.org"))

(generate-html
 (format "%s%s" *clamps-doc-dir*
         "/clamps.org"))

(generate-html
 (format "%s%s" *clamps-doc-dir*
         "/clamps-dictionary.org"))
