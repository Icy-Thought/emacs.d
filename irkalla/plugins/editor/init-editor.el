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
  :hook
  ((prog-mode . format-all-mode)
   (prog-mode . format-all-ensure-formatter)))

;; Code linting
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(provide 'init-editor)
