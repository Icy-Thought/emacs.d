;;; my-magit.el --- Magical Git Interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package magit
  :if (executable-find "git")
  :commands (magit)
  :init
  ;; :HACK| Magit complains a lot about Transient...
  (setopt elpaca-ignored-dependencies
          (delq 'transient elpaca-ignored-dependencies))
  :custom
  (magit-refs-show-commit-count 'all)
  (magit-save-repository-buffers 'dontask)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package magit-file-icons
  :after (magit)
  :hook (magit-mode . magit-file-icons-mode)
  :custom
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))

;; :NOTE| Provide a summary of repository TODO's in Magit buffer

(use-package hl-todo
  :ensure (hl-todo :version (lambda (_) "3.6.0"))) ;; elpaca

(use-package magit-todos
  :after (magit)
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-recursive t)
  (magit-todos-depth 10)
  (magit-todos-exclude-globs '(".git/" "*.html"))
  (magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-scanner #'magit-todos--scan-with-rg))

;; :NOTE| Provide git blame information inside buffers

(use-package blamer
  :if (executable-find "git")
  :commands (blamer-show-posframe-commit-info)
  :custom-face
  (blamer-face ((t (:background nil :height 125 :italic t))))
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-view 'overlay-right)
  (blamer-type 'visual)
  (blamer-max-commit-message-length 70)
  (blamer-force-truncate-long-line nil)
  (blamer-author-formatter " ✎ %s ")
  (blamer-commit-formatter "● \'%s\' ● "))

;; :NOTE| Highlight changes of the active buffer

(use-package git-gutter
  :if (executable-find "git")
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "┃")
  (git-gutter:added-sign "┃")
  (git-gutter:deleted-sign "┃")
  (git-gutter:unchanged-sign ""))

(provide 'my-magit)
