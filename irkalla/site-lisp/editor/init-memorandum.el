;;; init-memorandum.el --- Recalling Auto-Saved Information -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Memorizing our last known locations, history and being able to version-controll it is always a nice thing to have.

;;; Code:

(use-package savehist
  :elpaca nil
  :hook ((prog-mode text-mode) . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (history-length 1000)
  (savehist-autosave-interval 60)
  (savehist-file (no-littering-expand-var-file-name "savehist"))
  (savehist-save-minibuffer-history nil))

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
