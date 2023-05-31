;;; editor/init-orgmode.el -*- lexical-binding: t -*-

(defgroup irkalla-orgmode '()
  "The superior document format."
  :tag "Irkalla OrgMode"
  :group 'irkalla)

(defun irkalla/org-electric-dollar nil
  "Inserts \\( \\) when $, and replaces it with \\[ \\] when $$."
  (interactive)
  (if (and (looking-at "\\\\)")
           (looking-back "\\\\("))
      (progn (delete-char 2)
             (delete-char -2)
             (insert "\\[\\]"))
    (insert "\\(\\)")
    (backward-char 2)))

(use-package org
  :elpaca nil
  :hook ((org-mode org-babel-after-execute) . org-display-inline-images)
  :general (org-mode-map
            :states '(emacs insert normal)
            "$" #'irkalla/org-electric-dollar
            "C-<return>" #'org-ctrl-c-ret
            "M-<return>" #'org-edit-special)
  :config
  (let ((latex-dir (concat user-emacs-cache-directory "latex-preview")))
    (unless (file-directory-p latex-dir)
      (mkdir latex-dir t))
    (setq-default org-preview-latex-image-directory latex-dir))

  (setq-default org-latex-preview-options
                (progn (plist-put org-format-latex-options :background "Transparent")
                       (plist-put org-format-latex-options :scale 2.5)
                       (plist-put org-format-latex-options :zoom 1.15)))
  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 2)
  (org-cycle-include-plain-lists 'integrate)
  (org-ellipsis "…")
  (org-export-coding-system 'utf-8)
  (org-export-preserve-breaks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native))
  (org-insert-heading-respect-content t)
  (org-latex-tables-centered t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-tags-column 0)

  ;; Source blocks
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config (set-face-attribute 'org-modern-symbol nil :family "DejaVu Sans")
  :custom
  ;; Settings replaced by ~svg-tag-mode
  (org-modern-tag nil)
  (org-modern-todo nil))

(use-package org-roam
  :after org
  :general
  (irkalla/comma-lead-keydef
    :keymaps 'org-mode-map
    "o r l" '(org-roam-buffer-toggle  :which-key "Toggle Org-Roam on buffer")
    "o r n" '(org-roam-node-find      :which-key "Toggle Org-Roam on buffer")
    "o r g" '(org-roam-graph          :which-key "Toggle Org-Roam on buffer")
    "o r i" '(org-roam-node-insert    :which-key "Toggle Org-Roam on buffer")
    "o r c" '(org-roam-capture        :which-key "Toggle Org-Roam on buffer"))
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

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; :NOTE| Automatically render our LaTeX previews
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;; :NOTE| Auto-generate TOC (=:TOC:=) for our buffers
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable)
  :custom (toc-org-max-depth 3))

(provide 'init-orgmode)
