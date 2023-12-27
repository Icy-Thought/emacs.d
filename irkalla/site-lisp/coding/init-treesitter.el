;;; init-treesitter.el --- Treesitter: Quick Syntax Highlight -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Aesthetically wonderful addition to our Emacs editing environment.

;;; Code:

(use-package tree-sitter
  :elpaca nil
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom (font-lock-maximum-decoration t))

(use-package treesit-auto
  :hook (prog-mode . global-treesit-auto-mode)
  :custom (treesit-auto-install nil)
  :config
  (advice-add 'org-src-get-lang-mode
              :filter-return (lambda (mode)
                               (pcase (assoc mode major-mode-remap-alist)
                                 (`(,mode . ,ts-mode) ts-mode) (_ mode))))
  (treesit-auto-add-to-auto-mode-alist))

(use-package ts-fold
  :elpaca (:host github :repo "emacs-tree-sitter/ts-fold")
  :hook (tree-sitter-after-on . ts-fold-mode))

(use-package combobulate
  :elpaca (:host github :repo "mickeynp/combobulate")
  :hook (tree-sitter-after-on . combobulate-mode)
  :pretty-hydra
  ((:title (pretty-hydra-title "──｢ Coding: Combobulate ｣──" 'devicon "nf-dev-code")
           :color teal :quit-key "q")
   ("Interactive"
     (("o" combobulate "Combobulate")))))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
