;;; call with:
;;; emacs -q --eval "(defvar *clamps-doc-dir* \"`pwd`\")" --batch --load generate-html.el
;;; (defvar *clamps-doc-dir*)
;;; (setq *clamps-doc-dir* "/home/orm/work/programmieren/lisp/clamps/doc")
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/"))
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/htmlize-20240527.1456"))
(add-to-list 'load-path (format "%s%s" *clamps-doc-dir* "/../extra/elisp/rainbow-delimiters-20210515.1254"))

(load (format "%s%s" *clamps-doc-dir* "/../extra/elisp/font-lock-add-ons.el"))

(require 'org)
(require 'ox)
(require 'ox-html)
(require 'htmlize)
(require 'font-lock)
(require 'htmlize-autoloads)
(require 'rainbow-delimiters)

(setq org-html-htmlize-output-type 'css)

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
(setq org-export-allow-bind-keywords t)
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

;; (setq org-export-with-broken-links t)
;; (generate-html
;;  (format "%s%s" *clamps-doc-dir*
;;          "/fomus.org"))
