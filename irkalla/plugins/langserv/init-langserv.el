;;; langserv/init-langserv.el -*- lexical-binding: t -*-

;; Language Modes
(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package elpy
  :mode "\\.py\\'"
  :init
  (elpy-enable))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (add-hook 'rust-mode-hook 'eglot-ensure)
  :custom
  (rust-format-on-save t))

(provide 'init-langserv)
