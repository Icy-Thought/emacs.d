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
      vc-follow-symlinks t
      vc-make-backup-files t
      version-control t)

;; Grouped (setq-default) settings
(setq-default display-line-number-mode t
              display-line-numbers-type 'relative)

;; History: retention of undo's!
(setq history-delete-duplicates t
      prescient-history-length 1000
      history-length 1000)

;; Backups: retention and age..
(setq backup-by-copying t
      delete-by-moving-to-trash t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 3)

;; Completion: minor settings
(setq completions-detailed t
      completion-ignore-case t
      tab-always-indent t
      tab-first-completion 'word-or-paren-or-punct)

;; Decoration: minor settings
(setq truncate-lines t
      truncate-string-ellipsis "↴"
      window-combination-resize t
      x-stretch-cursor t)

(global-display-line-numbers-mode)

;; Indentation: 2 -> 4
(setq indent-tabs-mode nil                   ; tabs -> spaces
      electric-indent-inhibit t
      standard-indent 4
      tab-width 4)

;; Logical changes.. (🫠)
(setq-default buffer-file-coding-system 'utf-8-unix
              default-buffer-file-coding-system 'utf-8-unix
              echo-keystrokes 0.02)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-subword-mode 1)
(save-place-mode 1)

;; Smooth scrolling (Emacs >= 29)
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; (def) Toggle frame -> transparent
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(defun irkalla/toggle-frame-transparency ()
  "Toggle the frame transparency of on demand!"
  (interactive)
  (let ((alpha-value
         (if (equal (frame-parameter nil 'alpha-background) 100) 85
           100)))
    (set-frame-parameter nil 'alpha-background alpha-value)
    (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))))

(provide 'init-options)
