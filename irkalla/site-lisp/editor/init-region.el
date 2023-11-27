;;; init-region.el --- Regional-related Customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Indentations, region selection and more changes are to be displaced into this folder.

;;; Code:

(use-package ialign
  :general
  (irkalla/comma-lead-keydef
    :states 'visual :keymaps '(prog-mode-map text-mode-map)
    "a"   '(:ignore t :which-key "Alignment Control")
    "a r" '(ialign    :which-key "Align -> RegExp")))

(use-package indent-bars
  :elpaca (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :custom (indent-bars-zigzag nil))

(use-package expand-region
  :general (:states 'visual :keymaps '(prog-mode-map text-mode-map)
                    ")" 'er/expand-region
                    "(" 'er/contract-region))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package parens
  :elpaca nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery nil)
  (show-paren-when-point-inside-paren nil))

(use-package subword
  :elpaca nil
  :hook ((prog-mode text-mode) . subword-mode))

(provide 'init-region)
;;; init-region.el ends here
