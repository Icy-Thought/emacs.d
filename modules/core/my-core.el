;;; my-core.el --- The Core of Irkalla Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :hook (after-init . pixel-scroll-precision-mode)
  :custom (scroll-preserve-screen-position t))

(use-feature time
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))

;; :NOTE| Time to require the modules

(require 'my-windows)
(require 'my-littering)
(require 'my-hydra)

(provide 'my-core)
