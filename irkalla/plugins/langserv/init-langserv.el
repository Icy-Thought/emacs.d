;;; langserv/init-langserv.el -*- lexical-binding: t -*-

;; Required before applying changes to eglot...
(require 'eglot)

;; Make writing lisp simpler!
(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :init
  (setq parinfer-rust-auto-download t))

;; Language Modes
(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false))))))  ;; disable stan
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :config (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-workspace-configuration
        '((:pyright . ((useLibraryCodeForTypes . t))))))

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :custom (rustic-lsp-client 'eglot)
  :config
  (defun irkalla/locate-cargo-toml (dir)
    "Locate the missing rust project Cargo."
    (if-let ((root (locate-dominating-file dir "Cargo.toml")))
        (list 'vc 'Git root)))

  (add-hook 'rust-mode-hook
            (lambda ()
              (add-to-list 'project-find-functions #'irkalla/locate-cargo-toml))))

(provide 'init-langserv)
