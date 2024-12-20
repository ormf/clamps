;;; Function needed by clamps-dictionary.org. This file gets loaded on
;;; startup of cl clamps. If clamps-dictionary.org should get updated
;;; when clamps is *not* running make sure, this file is loaded before
;;; starting multipage export of clamps-dictionary.org

(defvar *clamps-doc-symbols* (make-vector 63 0))
(defvar *clamps-overview-symbols* (make-vector 63 0))
(defvar *clamps-fomus-symbols* (make-vector 63 0))

(defun export-dict-to-clamps (data _backend info)
  (with-temp-buffer 
    (insert "(mapcar
     (lambda (entry)
       (let ((symbol (intern (car entry)
                     *common-music-symbols*)))
         (if (boundp symbol)
    	 (push (cadr entry) (symbol-value symbol))
           (set symbol (cdr entry)))))
     '(\n")
    (mapcar
     (lambda (entry)
       (insert
        (format "   (\"%s\" \"clamps-dict/%s\")\n"
                (org-html-element-title (car entry))
                (plist-get (cdr entry) :href))))
     (cl-remove-if
      (lambda (x) (= 1 (plist-get (cdr x) :relative-level)))
      (plist-get global-info :multipage-toc-lookup)))
    (insert "))\n")
    (write-region (point-min) (point-max) "../extra/elisp/clamps-dict.el"))
  (load "cm-dict.el")
  (load "clamps-dict.el")
  data)

(define-key sly-mode-map (kbd "\C-c\C-dc") 'cm-lookup)
(define-key sly-mode-map (kbd "C-.") 'clamps-hush)
(define-key sly-mode-map (kbd "C-c C-.") 'incudine-rt-stop)
(define-key sly-mode-map (kbd "C-c M-.") 'incudine-rt-start)
(define-key sly-mode-map (kbd "C-c t") 'test-midi)
