;;; init-icons.el --- Fontification & Icons -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Reducing text with icons is always a nice addition.

;;; Code:

(use-package nerd-icons
  :custom
  (nerd-icons-font-family (face-attribute 'default :family))
  (nerd-icons-scale-factors 1.25))

(use-package nerd-icons-completion
  :after (nerd-icons vertico)
  :hook (vertico-mode . nerd-icons-completion-mode))

(provide 'init-icons)
;;; init-icons.el ends here
