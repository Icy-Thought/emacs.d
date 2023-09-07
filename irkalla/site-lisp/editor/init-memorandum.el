;;; init-memorandum.el --- Recalling Auto-Saved Information -*- lexical-binding: t -*-

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
  (make-backup-files t)
  (undo-limit 6710886400) ;; 64mb
  (undo-outer-limit 1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.
  (undo-strong-limit 100663296)) ;; x 1.5 (96mb)

(use-package savehist
  :elpaca nil
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 60)
  (savehist-file (no-littering-expand-var-file-name "savehist"))
  (savehist-ignored-variables '(ement-room-message-history)))

(use-package save-place
  :elpaca nil
  :hook ((prog-mode text-mode) . save-place-mode)
  :custom
  (save-place-file (no-littering-expand-var-file-name "saveplace"))
  (save-place-forget-unreadable-files t))

(use-package vundo
  :general
  (irkalla/comma-lead-keydef
    "u" '(vundo :which-key "Visualize Undo"))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-memorandum)
;;; init-memorandum.el ends here
