;;; lisp/init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Non-categorized Options!
(setq auto-save-interval 50
      confirm-nonexistent-file-or-buffer nil
      enable-local-variables t
      enable-recursive-minibuffers t
      find-file-suppress-same-file-warnings t
      frame-resize-pixelwise t
      help-window-select t
      inhibit-startup-echo-area-message t
      kill-whole-line t
      Man-notify-method 'pushy
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      recenter-positions '(top middle bottom)
      remote-file-name-inhibit-locks t
      require-final-newline t
      ring-bell-function 'ignore
      search-whitespace-regexp nil
      sentence-end-double-space nil
      use-short-answers t
      use-dialog-box nil
      vc-follow-symlinks t
      vc-make-backup-files t
      version-control t)

;; Grouped (setq-default) settings
(use-package display-line-numbers
  :ensure nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config (setq-default display-line-numbers-type 'relative))

;; Backups: retention and age..
(setq backup-by-copying t
      delete-by-moving-to-trash t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 3)

;; Completion: minor settings
(setq completions-detailed t
      completion-ignore-case t
      tab-always-indent 'completion
      tab-first-completion 'word-or-paren-or-punct)

;; Decoration: minor settings
(setq truncate-lines t
      truncate-string-ellipsis "↴"
      window-combination-resize t
      x-stretch-cursor t)

;; Indentation: 2 -> 4 + tabs -> spaces
(setq indent-tabs-mode nil
      electric-indent-inhibit t
      standard-indent 4
      tab-width 4)

;; Logical changes.. (🫠)
(prefer-coding-system 'utf-8)
(setq-default echo-keystrokes 0.02)

(delete-trailing-whitespace)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-subword-mode t)
(save-place-mode t)

;; Smooth scrolling (Emacs >= 29)
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(provide 'init-options)
