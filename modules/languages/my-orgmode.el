;;; my-orgmode.el --- Org-Mode Documentation Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature org
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
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 2.5)
  (plist-put org-format-latex-options :zoom 1.15)
  :custom
  (org-agenda-files '("~/Workspace/memorandum/org-mode/agenda/init.org"))
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-include-plain-lists 'integrate)
  (org-cycle-separator-lines 2)
  (org-ellipsis "…")
  (org-export-coding-system 'utf-8)
  (org-export-preserve-breaks t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native))
  (org-insert-heading-respect-content t)
  (org-latex-tables-centered t)
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-tags-column 0)
  (org-list-allow-alphabetical t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)

  ;; Code blocks
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-auto-save-idle-delay auto-save-timeout)
  (org-edit-src-turn-on-auto-save t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively nil))

;; :NOTE| Control of Org source blocks and tangles

(use-feature ob
  :after (org)
  :hook (org-babel-after-execute . org-display-inline-images)
  :custom
  (org-babel-default-header-args
   '((:async   . "yes")
     (:cache   . "no")
     (:eval    . "never-export")
     (:exports . "both")
     (:hlines  . "no")
     (:noweb   . "yes")
     (:results . "output replace")
     (:session . "none")
     (:tangle .  "no")))
  (org-export-use-babel nil)
  (org-confirm-babel-evaluate nil)
  :config
  ;; :NOTE| https://emacs.stackexchange.com/a/20618
  (defun demand-babel-languages (orig-fun &rest args)
    "Load language if needed before executing a source block."
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      (apply orig-fun args)))

  (defun irkalla/org-execute-action ()
    ;; In a source block, call `org-babel-execute-src-block'.
    (interactive)
    (let ((context (org-element-context)))
      (pcase (org-element-type context)
        (`src-block
         ;; In a source block, call `org-babel-execute-src-block'.
         (org-babel-eval-wipe-error-buffer)
         (org-babel-execute-src-block current-prefix-arg))
        (`babel-call
         ;; In a `:+CALL:' block, call `org-babel-execute-maybe'.
         (call-interactively #'org-babel-execute-maybe))
        (`table-row
         ;; In a table or table-cell, call `org-table-next-row'.
         (call-interactively #'org-table-next-row))
        ((or `link `timestamp)
         ;; On a link or a timestamp, call `org-open-at-point'.
         (call-interactively #'org-open-at-point))
        (_
         ;; Fallback to evil standard command
         (call-interactively #'forward-line)))))

  (defun irkalla/tangle-config-file ()
    "Tangle Irkalla Emacs configuration file on save."
    (when (and (eq major-mode 'org-mode)
               (string= (buffer-file-name)
                        (expand-file-name "config.org" irkalla/underworld)))
      (org-babel-tangle)))

  (add-hook 'after-save-hook #'irkalla/tangle-config-file)

  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map (kbd "<return>") #'irkalla/org-execute-action))
  (advice-add 'org-babel-execute-src-block :around #'demand-babel-languages))

;; :NOTE| Auto-generated table of contents

(use-package toc-org
  :after (org)
  :hook (org-mode . toc-org-mode)
  :custom (toc-org-max-depth 3))

;; :NOTE| LaTeX rendering of equations on hover

(use-package org-fragtog
  :after (org)
  :hook (org-mode . org-fragtog-mode))

;; :NOTE| Super-charge Org agendas

(use-package org-super-agenda
  :after (org-agenda)
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '((:name "Today" :time-grid t :todo "TODAY")
     (:name "Important" :tag "bills" :priority "A")
     (:todo "WAITING" :order 8)
     (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING") :order 9)
     (:priority<= "B" :order 1))))

;; :NOTE| Modernize the aesthetics of Org buffers

(use-package org-modern
  :after (org)
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom-face (org-modern-symbol ((t (:family "DejaVu Sans"))))
  :custom
  ;; :NOTE| Settings replaced by svg-tag-mode
  (org-modern-tag nil)
  (org-modern-todo nil)
  (org-modern-block-name nil))

;; :NOTE| Easier grep of Org files

(use-package org-ql
  :after (org)
  :commands (org-ql-search))

;; :NOTE| Schedule your daily life through Org

(use-package org-timeblock
  :after (org)
  :commands (org-timeblock))

;; :NOTE| Org based Zettlekasten System

(use-package org-roam
  :after (org)
  :commands (org-roam-graph)
  :custom
  (org-roam-directory (file-truename "~/Workspace/memorandum/org-mode/org-roam"))
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
  :after (org-roam)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; :NOTE| Cite your sources!!

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :custom (citar-bibliography '("~/Workspace/memorandum/references.bib")))

(use-package citar-embark
  :after (citar embark)
  :hook (org-mode . citar-embark-mode)
  :config (setopt citar-at-point-function 'embark-act))

(provide 'my-orgmode)
