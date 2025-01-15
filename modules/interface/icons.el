;;; icons.el --- Elegant Symbols To Sprinkle Emacs With -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package nerd-icons :demand t
  :custom
  (nerd-icons-font-family
   (when (featurep 'fontaine)
     (plist-get (fontaine--get-preset-properties 'default) :default-family)))
  (nerd-icons-scale-factor 1.05))

(use-package nerd-icons-completion
  :after (nerd-icons)
  :config (nerd-icons-completion-mode))

(provide 'irkalla/icons)
