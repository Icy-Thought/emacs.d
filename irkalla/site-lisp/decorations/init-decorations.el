;;; init-decorations.el --- Decoration-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; We cannot allow Emacs to co-exist with aesthetically modified applications without applying some form of pleasantaries to it!

;;; Code:

(use-package autothemer
  :init (load-theme 'catppuccin-mocha :no-confirm))

;; :NOTE| Replace several symbols with prettier alternatives
(use-feature prettify-symbols
  :hook (emacs-lisp-mode . prettify-symbols-mode)
  :custom (prettify-symbols-unprettify-at-point 'right-edge))

;; :NOTE| Import the custom modules
(require 'init-typeface)
(require 'init-icons)
(require 'init-svg-tags)
(require 'init-modeline)

(provide 'init-decorations)
;;; init-decorations.el ends here
