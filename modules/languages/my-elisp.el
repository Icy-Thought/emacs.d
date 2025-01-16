;;; my-elisp.el --- Elisp Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :hook (emacs-lisp-mode . prettify-symbols-mode)
  :config (setopt prettify-symbols-unprettify-at-point 'right-edge))

(provide 'my-elisp)
