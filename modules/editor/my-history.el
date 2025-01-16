;;; my-history.el --- History Should Become Useful -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :config
  (setopt auto-save-interval 200
          auto-save-timeout 30
          history-length 1000
          kept-new-versions 7
          kept-old-versions 3
          backup-by-copying t
          delete-by-moving-to-trash t
          delete-old-versions t
          history-delete-duplicates t
          make-backup-files t))

(use-feature savehist
  :defer 1
  :config (savehist-mode)
  :custom
  (savehist-autosave-interval 60)
  (savehist-file (no-littering-expand-var-file-name "savehist"))
  (savehist-additional-variables '(command-history evil-jumps-history))
  (savehist-ignored-variables '(ement-room-message-history)))

(use-feature saveplace
  :hook ((prog-mode text-mode) . save-place-mode)
  :custom
  (save-place-file (no-littering-expand-var-file-name "saveplace"))
  (save-place-forget-unreadable-files t))

;; :NOTE| Wrapper around built-in undo system

(use-package undo-fu
  :if (>= emacs-major-version 29)
  :config
  (setopt undo-no-redo      t
          undo-limit        (* 128 1024 1024)
          undo-outer-limit  (* 128 1024 1024)
          undo-strong-limit (* 256 1024 1024)))

(use-package undo-fu-session
  :after (undo-fu)
  :hook ((prog-mode text-mode) . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (no-littering-expand-var-file-name "undo-fu-session/"))
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; :NOTE| Tree-like undo history UI

(use-package vundo
  :commands (vundo)
  :bind (("C-c u" . vundo))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols))

(provide 'my-history)
