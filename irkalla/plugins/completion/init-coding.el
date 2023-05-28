;;; completion/init-coding.el -*- lexical-binding: t -*-

(defgroup irkalla-coding '()
  "General programming language conf for Emacs"
  :tag "Irkalla Coding"
  :group 'irkalla)

;; Required before applying changes to eglot...
(use-package eglot
  :general (irkalla/space-lead-keydef
             :keymaps 'eglot-mode-map
             "l a"       '(eglot-code-actions    :which-key "Perform code-actions on buffer")
             "l r"       '(eglot-rename          :which-key "Rename $SYMBOL to a newer name")
             "l <space>" '(eglot-format          :which-key "Format active buffer")
             "l ?"       '(xref-find-references  :which-key "Find references of identifier at cursor")
             "l d"       '(xref-find-definitions :which-key "Find definition of identifier at cursor")
             "l /"       '(xref-find-apropos     :which-key "Find meaningful $SYMBOLS which matches pattern"))
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
  :general (irkalla/comma-lead-keydef
             :keymaps 'markdown-mode-map
             "m d" '(markdown-do :which-key "Perform a senile action based on context")))

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
  :custom (rustic-lsp-client 'eglot)
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  (defun irkalla/locate-cargo-toml (dir)
    "Locate the missing rust project Cargo."
    (if-let ((root (locate-dominating-file dir "Cargo.toml")))
        (list 'vc 'Git root)))
  (add-hook 'rust-mode-hook (lambda ()
                              (add-to-list 'project-find-functions #'irkalla/locate-cargo-toml))))

(use-package typst-mode
  :elpaca (:host github :repo "Ziqi-Yang/typst-mode.el")
  :mode ("\\.typ\\'" . typst-mode))

(use-package yuck-mode) ;; For our EWW :P

(provide 'init-coding)
