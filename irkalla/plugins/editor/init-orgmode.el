;;; editor/init-orgmode.el -*- lexical-binding: t -*-

(defgroup irkalla-orgmode '()
  "The superior document format."
  :tag "Irkalla OrgMode"
  :group 'irkalla)

(use-package org
  :defer t
  :hook ((org-mode org-babel-after-execute) . org-display-inline-images)
  :custom
  (let ((latex-dir (concat user-emacs-cache-directory "latex-preview")))
    (unless (file-directory-p latex-dir)
      (mkdir latex-dir t))
    (org-preview-latex-image-directory latex-dir))

  (custom-set-variables
   '(org-format-latex-options
     '(:foreground default
       :background default
       :scale 2.0
       :html-foreground "Black"
       :html-background "Transparent"
       :html-scale 2.0
       :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-include-plain-lists 'integrate)
  (org-ellipsis "…")
  (org-export-preserve-breaks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native))
  (org-insert-heading-respect-content t)
  (org-latex-tables-centered t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-tags-column 0))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config (set-face-attribute 'org-modern-symbol nil :family "DejaVu Sans"))

(use-package org-roam
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-directory (file-truename "~/org/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
               "%<%Y%m%d%H%M%S>-${slug}.org"
               ,(let ((options '("#+options: _:{}"
                                 "#+options: ^:{}"
                                 "#+startup: latexpreview"
                                 "#+startup: entitiespretty"
                                 "#+startup: inlineimages"
                                 "#+title: ${title}")))
                  (mapconcat 'identity options "\n")))
      :unnarrowed t)))
  (org-roam-node-display-template "${title}"))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable)
  :custom (toc-org-max-depth 3))

(provide 'init-orgmode)
