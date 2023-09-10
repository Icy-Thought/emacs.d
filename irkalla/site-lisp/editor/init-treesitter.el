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

(use-package treesit-auto
  :demand t
  :after tree-sitter
  :hook (prog-mode . global-treesit-auto-mode))

(use-package ts-fold
  :elpaca (:host github :repo "emacs-tree-sitter/ts-fold")
  :hook (prog-mode . ts-fold-mode))

(use-package combobulate
  :elpaca (:host github :repo "mickeynp/combobulate")
  :hook (tree-sitter-after-on . combobulate-mode)
  :general
  (irkalla/space-lead-keydef
    "l c" '(combobulate :which-key "Combobulate")))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
