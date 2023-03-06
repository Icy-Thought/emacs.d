;;; langserv/init-langserv.el -*- lexical-binding: t -*-

(use-package eglot
  :hook
  (nix-mode . eglot-ensure)
  (rust-mode-hook . eglot-ensure))

;; Make writing lisp simpler!
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t))

;; Language Modes
(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package nix-mode
  :mode "\\.nix\\'"
  :config (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(use-package elpy
  :mode "\\.py\\'"
  :init (elpy-enable))

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(provide 'init-langserv)
