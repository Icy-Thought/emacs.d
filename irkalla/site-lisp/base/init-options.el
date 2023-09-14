;;; init-options.el --- Opinionated Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Random options that I have throughout my Emacs journey considered to be useful.

;;; Code:

(setq-default ad-redefinition-action 'accept
              auto-mode-case-fold nil
              auto-save-list-file-prefix nil
              command-line-x-option-alist nil
              create-lockfiles nil
              default-input-method nil
              fast-but-imprecise-scrolling t
              ffap-machine-p-known 'reject
              frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              idle-update-delay 1.0
              inhibit-compacting-font-caches t
              inhibit-default-init t
              inhibit-splash-screen t
              inhibit-startup-buffer-menu t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-message t
              inhibit-startup-screen t
              load-prefer-newer noninteractive
              mode-line-format nil
              redisplay-skip-fontification-on-input t
              select-active-regions 'only
              site-run-file nil
              use-dialog-box nil
              use-file-dialog nil
              use-short-answers t
              vc-follow-symlinks t)

;; :NOTE| Sanner defautlt for Emacs encoding system
(set-default-coding-systems 'utf-8)

(provide 'init-options)
;;; init-options.el ends here
