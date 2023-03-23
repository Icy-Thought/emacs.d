;;; lisp/init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

(use-package emacs
  :ensure nil
  :hook (before-save . 'whitespace-cleanup)
  :init
  (prefer-coding-system 'utf-8)
  (delete-trailing-whitespace)
  (global-auto-revert-mode t)
  (global-hl-line-mode t)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-subword-mode t)
  (save-place-mode t)
  :custom
  (Man-notify-method 'pushy)
  (auto-save-interval 50)
  (confirm-nonexistent-file-or-buffer nil)
  (echo-keystrokes 0.02)
  (enable-local-variables t)
  (enable-recursive-minibuffers t)
  (find-file-suppress-same-file-warnings t)
  (frame-resize-pixelwise t)
  (help-window-select t)
  (inhibit-startup-echo-area-message t)
  (kill-whole-line t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (recenter-positions '(top middle bottom))
  (remote-file-name-inhibit-locks t)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (search-whitespace-regexp nil)
  (sentence-end-double-space nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (vc-make-backup-files t)
  (version-control t))

;; Relative number line
(use-package display-line-numbers
  :ensure nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config (setq-default display-line-numbers-type 'relative))

;; Logical changes.. (🫠)
(provide 'init-options)
