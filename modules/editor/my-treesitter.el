;;; my-treesitter.el --- A Faster Syntax Highlighter -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package treesit-auto
  :custom (treesit-auto-install nil)
  :config
  (advice-add 'org-src-get-lang-mode
              :filter-return (lambda (mode)
                               (pcase (assoc mode major-mode-remap-alist)
                                 (`(,mode . ,ts-mode) ts-mode) (_ mode))))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; :NOTE| Folding of code-blocks

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . (lambda ()
                       (when (and (treesit-available-p)
                                  (treesit-parser-list))
                         (treesit-fold-mode t)))))

;; :NOTE| Structural editing & navigation

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :commands (combobulate)
  :hook (tree-sitter-after-on . combobulate-mode))

(provide 'my-treesitter)
