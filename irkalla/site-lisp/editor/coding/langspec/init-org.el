;;; init-org.el --- Org-Mode: The Superior Document Format -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; It does not get more fluid than Org-Mode! Using another documentation format does not seeem like a logical choice
;; after being exposed to it.

;;; Code:

(use-package org
  :elpaca nil
  :preface
  (defun irkalla/org-electric-dollar ()
    "Inserts \\( \\) when $, and replaces it with \\[ \\] when $$."
    (interactive)
    (if (and (looking-at "\\\\)")
             (looking-back "\\\\("))
        (progn (delete-char 2)
               (delete-char -2)
               (insert "\\[\\]"))
      (insert "\\(\\)")
      (backward-char 2)))
  :hook (org-mode . org-display-inline-images)
  :custom-face
  (org-document-title ((t (:height 1.50))))
  (org-level-1        ((t (:inherit outline-1 :height 1.25))))
  (org-level-2        ((t (:inherit outline-2 :height 1.15))))
  (org-level-3        ((t (:inherit outline-3 :height 1.12))))
  (org-level-4        ((t (:inherit outline-4 :height 1.09))))
  (org-level-5        ((t (:inherit outline-5 :height 1.06))))
  :config
  (setopt org-directory "~/Workspace/memorandum/org-mode")

  (with-eval-after-load 'evil
    (evil-define-key 'insert org-mode-map (kbd "$") #'irkalla/org-electric-dollar))

  ;; :NOTE| Move our LaTeX previews to cache dir
  (let ((latex-dir (no-littering-expand-var-file-name "latex-preview/")))
    (unless (file-directory-p latex-dir)
      (mkdir latex-dir t))
    (setopt org-preview-latex-image-directory latex-dir))

  ;; :NOTE| Change the aesthetics of our LaTeX previews
  (setopt org-latex-preview-options
          (progn (plist-put org-format-latex-options :background "Transparent")
                 (plist-put org-format-latex-options :scale 2.5)
                 (plist-put org-format-latex-options :zoom 1.15)))
  :custom
  (org-agenda-files '("~/Workspace/memorandum/org-mode/agenda/init.org"))
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-include-plain-lists 'integrate)
  (org-cycle-separator-lines 2)
  (org-edit-src-auto-save-idle-delay 5)
  (org-ellipsis "…")
  (org-export-coding-system 'utf-8)
  (org-export-preserve-breaks t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native))
  (org-image-actual-width (truncate (* (window-pixel-width) 0.8)))
  (org-insert-heading-respect-content t)
  (org-latex-tables-centered t)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-tags-column 0)

  ;; Code blocks
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively nil))

;; :NOTE| Automatic rendering of LaTeX code-blocks
(use-package org-fragtog
  :requires (org)
  :hook (org-mode . org-fragtog-mode))

;; :NOTE| Automatic generation of ToC
(use-package toc-org
  :requires (org)
  :hook (org-mode . toc-org-enable)
  :custom (toc-org-max-depth 3))

;; :NOTE| Modernizing our Org-Mode buffers
(use-package org-modern
  :requires (org)
  :hook (org-mode . org-modern-mode)
  :custom-face (org-modern-symbol ((t (:family "DejaVu Sans"))))
  :custom
  ;; :NOTE| Settings replaced by svg-tag-mode
  (org-modern-tag nil)
  (org-modern-todo nil)
  (org-modern-block-name nil))

(use-package org-ql
  :requires (org)
  :commands (org-ql-search))

(use-package org-timeblock
  :requires (org)
  :commands (org-timeblock))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define org-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Org-Mode ｣──" 'sucicon "nf-custom-orgmode")
            :color teal :quit-key "q")
    ("Buffer"
     (("e" org-edit-special "Specialized Edit")
      ("t" org-babel-tangle "Tangle")
      ("c" org-capture      "Capture"))
     "Project"
     (("/" org-ql-search    "Search TAG Org Files"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Markup"
     (("o" (if (eq major-mode 'org-mode)
               (org-hydra/body)
             (message "You are not in an Org buffer.")) "Org-Mode")))))

(provide 'init-org)
;;; init-org.el ends here
