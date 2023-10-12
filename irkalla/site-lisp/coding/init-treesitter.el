;;; init-treesitter.el --- Treesitter: Quick Syntax Highlight -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Aesthetically wonderful addition to our Emacs editing environment.

;;; Code:

(use-package treesit
  :elpaca nil
  :custom (treesit-font-lock-level 4))

(use-package tree-sitter
  :elpaca nil
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :elpaca nil
  :after tree-sitter
  :diminish tree-sitter-mode)

(use-package ts-fold
  :elpaca (:host github :repo "emacs-tree-sitter/ts-fold")
  :hook (tree-sitter-after-on . ts-fold-mode))

(use-package combobulate
  :elpaca (:host github :repo "mickeynp/combobulate")
  :hook (tree-sitter-after-on . combobulate-mode)
  :general
  (irkalla/space-lead-keydef
    "l c" '(combobulate :which-key "Combobulate")))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
