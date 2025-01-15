;;; files.el --- Managing Files & Directories -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Project based configurations ought to be respected

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

;; :NOTE| Load directory environments

(use-package direnv
  :hook ((prog-mode text-mode) . direnv-mode)
  :config (add-to-list 'warning-suppress-types '(direnv))
  :custom (direnv-always-show-summary nil))

;; :NOTE| Scripts -> executable on save

(use-feature executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; :NOTE| Update opened buffers on change

(use-feature autorevert
  :hook ((prog-mode text-mode) . auto-revert-mode)
  :custom
  (auto-revert-interval 1)
  (auto-revert-notify t)
  (auto-revert-verbose t))

;; :NOTE| Display file differences

(use-feature ediff
  :hook((ediff-prepare-buffer . outline-show-all)
        (ediff-quit . winner-undo))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(provide 'irkalla/files)
