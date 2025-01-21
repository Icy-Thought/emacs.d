;;; my-modeline.el --- A Status-Bar For Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package doom-modeline
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 0)
  (doom-modeline-buffer-file-name 'relative-to-project))

(provide 'my-modeline)
