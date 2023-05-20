;;; langserv/init-langserv.el -*- lexical-binding: t -*-

;; Require org-babel modules
(require 'init-babel)

;; Required before applying changes to eglot...
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c ." . eglot-code-actions)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format)
              ("M-?" . xref-find-references)
              ("M-." . xref-find-definitions)
              ("C-c x a" . xref-find-apropos)
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f d" . flymake-show-project-diagnostics))
  :config (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :custom
  (eglot-autoshutdown t)
  (eglot-menu-string "LSP")
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-confirm-server-initiated-edits nil))

;; Make writing lisp simpler!
(use-package parinfer-rust-mode
  :hook (emacs-lisp-mode . parinfer-rust-mode)
  :custom (parinfer-rust-auto-download t))

;; Language Modes
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false)))))))  ;; disable stan

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :hook (nix-mode . eglot-ensure))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-workspace-configuration
        '((:pyright . ((useLibraryCodeForTypes . t))))))

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(rust-mode . rustic-mode))

  (defun irkalla/locate-cargo-toml (dir)
    "Locate the missing rust project Cargo."
    (if-let ((root (locate-dominating-file dir "Cargo.toml")))
        (list 'vc 'Git root)))

  (add-hook 'rust-mode-hook
            (lambda ()
              (add-to-list 'project-find-functions #'irkalla/locate-cargo-toml))))

(use-package typst-mode
  :elpaca (:host github :repo "Ziqi-Yang/typst-mode.el")
  :mode ("\\.typ\\'" . typst-mode))

(use-package yuck-mode) ;; For our EWW :P

(provide 'init-langserv)
