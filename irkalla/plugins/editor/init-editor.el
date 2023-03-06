;;; editor/init-editor.el -*- lexical-binding: t -*-

;; Require custom editor modules:
(require 'init-evil)
(require 'init-orgmode)
(require 'init-indentation)

;; Smarter lisp parents
(use-package smartparens
  :init
  (smartparens-global-mode +1))

;; Auto-format code!
(use-package format-all
  :config
  :hook ((prog-mode . format-all-mode)
         (prog-mode . format-all-ensure-formatter))
  :config
  (setq-default format-all-formatters
                '(("Nix" nixfmt)
                  ("Haskell" stylish-haskell)
                  ("Rust" rustfmt))))

;; Code linting
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Spelling
(use-package flyspell
  :defer 1
  :hook
  ((markdown-mode org-mode text-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-default-dictionary "en_US"))

(provide 'init-editor)
