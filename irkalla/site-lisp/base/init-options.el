;;; init-options.el --- Opinionated Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Random options that I have throughout my Emacs journey considered to be useful.

;;; Code:

;; :NOTE| Debugging and error handling
(setq-default
 ad-redefinition-action 'accept
 debug-on-error init-file-debug
 jka-compr-verbose init-file-debug)

;; :NOTE| User interface and startup
(setq-default
 auto-mode-case-fold nil
 blink-cursor-mode nil
 echo-keystrokes 0.02
 fast-but-imprecise-scrolling t
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-message t
 inhibit-startup-screen t
 use-dialog-box nil
 use-file-dialog nil)

;; :NOTE| Display and fonts
(setq-default
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t
 idle-update-delay 1.0
 inhibit-compacting-font-caches t
 mode-line-format nil
 redisplay-skip-fontification-on-input t)

;; :NOTE| File handling and version control
(setq-default
 auto-save-list-file-prefix nil
 create-lockfiles nil
 package-enable-at-startup nil
 use-short-answers t
 vc-follow-symlinks t)

;; :NOTE| Miscellaneous
(setq-default
 command-line-x-option-alist nil
 default-input-method nil
 ring-bell-function 'ignore
 select-active-regions 'only
 load-prefer-newer noninteractive)

(set-default-coding-systems 'utf-8)

(provide 'init-options)
;;; init-options.el ends here
