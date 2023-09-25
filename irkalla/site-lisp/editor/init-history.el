;;; init-history.el --- Recalling Auto-Saved Information -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Memorizing our last known locations, history and being able to version-controll it is always a nice thing to have.

;;; Code:

(use-package emacs
  :elpaca nil
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
  :elpaca nil
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 60)
  (savehist-file (no-littering-expand-var-file-name "savehist"))
  (savehist-additional-variables '(command-history evil-jumps-history))
  (savehist-ignored-variables '(ement-room-message-history)))

(use-package save-place
  :elpaca nil
  :hook ((prog-mode text-mode) . save-place-mode)
  :custom
  (save-place-file (no-littering-expand-var-file-name "saveplace"))
  (save-place-forget-unreadable-files t))

(use-package undo-fu
  :demand t
  :config
  (setq-default undo-limit 400000           ; 400kb (default is 160kb)
                undo-outer-limit 48000000   ; 48mb  (default is 24mb)
                undo-strong-limit 3000000)) ; 3mb   (default is 240kb)

(use-package undo-fu-session
  :after undo-fu
  :hook ((prog-mode text-mode) . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (no-littering-expand-var-file-name "undo-fu-session/"))
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :general
  (irkalla/comma-lead-keydef
    "u" '(vundo :which-key "Visualize Undo"))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-history)
;;; init-history.el ends here
