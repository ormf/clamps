(require 'ol)

(org-link-set-parameters "dict"
                         :follow #'org-dict-open
                         :export #'org-dict-export
                         :store #'org-dict-store-link)

(org-link-set-parameters "clamps"
                         :follow #'org-clamps-open
                         :export #'org-clamps-export
                         :store #'org-clamps-store-link)

(org-link-set-parameters "overview"
                         :follow #'org-overview-open
                         :export #'org-overview-export
                         :store #'org-overview-store-link)

(defcustom org-dict-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-dict-open (path _)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-dict-command path))

(defun org-dict-store-link (&optional _interactive?)
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link.
    (let* ((page (org-man-get-page-name))
           (link (concat "dict:" page))
           (description (format "Link to clamps dict page for %s" page)))
      (org-link-store-props
       :type "dict"
       :link link
       :description description))))

(defun org-dict-export (link description format _)
  "Export a clamps dict link from Org files."
  (message "dict link: \"%s\" \"%s\" \"%s\"" link description format)
  (let ((path (format "../%s"
                      (car
                       (symbol-value
                        (intern-soft link *common-music-symbols*)))))
        (desc (or description link)))
    (pcase format
      (`html (format "<a href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defcustom org-clamps-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-clamps-open (path _)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-clamps-command path))

(defun org-clamps-store-link (&optional _interactive?)
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link.
    (let* ((page (org-man-get-page-name))
           (link (concat "clamps:" page))
           (description (format "Link to clamps clamps page for %s" page)))
      (org-link-store-props
       :type "clamps"
       :link link
       :description description))))

(defun org-clamps-export (link description format _)
  "Export a clamps clamps link from Org files."
  (message "clamps link: \"%s\" \"%s\" \"%s\"" link description format)
  (let ((path (format "../%s"
                      (car
                       (symbol-value
                        (intern-soft link *clamps-doc-symbols*)))))
        (desc (or description link)))
    (pcase format
      (`html (format "<a href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))

(defcustom org-overview-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(defun org-overview-open (path _)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-overview-command path))

(defun org-overview-store-link (&optional _interactive?)
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link.
    (let* ((page (org-man-get-page-name))
           (link (concat "overview:" page))
           (description (format "Link to clamps overview page for %s" page)))
      (org-link-store-props
       :type "overview"
       :link link
       :description description))))

(defun org-overview-export (link description format _)
  "Export a clamps overview link from Org files."
  (message "overview link: \"%s\" \"%s\" \"%s\"" link description format)
  (let ((path (format "../%s"
                      (car
                       (symbol-value
                        (intern-soft link *clamps-overview-symbols*)))))
        (desc (or description link)))
    (pcase format
      (`html (format "<a href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (_ path))))





(provide 'clamps-links)
;; clamps-links ends here

