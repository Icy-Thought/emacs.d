;;; init-history.el --- Recalling Auto-Saved Information -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Memorizing our last known locations, history and being able to version-controll it is always a nice thing to have.

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (auto-save-interval 200)
  (auto-save-timeout 30)
  (backup-by-copying t)
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (history-delete-duplicates t)
  (history-length 1000)
  (kept-new-versions 7)
  (kept-old-versions 3)
  (make-backup-files t))

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 60)
  (savehist-file (no-littering-expand-var-file-name "savehist"))
  (savehist-additional-variables '(command-history evil-jumps-history))
  (savehist-ignored-variables '(ement-room-message-history)))

(use-package save-place
  :ensure nil
  :hook ((prog-mode text-mode) . save-place-mode)
  :custom
  (save-place-file (no-littering-expand-var-file-name "saveplace"))
  (save-place-forget-unreadable-files t))

(use-package undo-fu
  :if (>= emacs-major-version 29)
  :demand t
  :config
  (setopt undo-limit        (* 128 1024 1024)
          undo-outer-limit  (* 128 1024 1024)
          undo-strong-limit (* 256 1024 1024)))

(use-package undo-fu-session
  :requires (undo-fu)
  :hook ((prog-mode text-mode) . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (no-littering-expand-var-file-name "undo-fu-session/"))
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :commands (vundo)
  :bind (("C-c u" . vundo))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-history)
;;; init-history.el ends here
