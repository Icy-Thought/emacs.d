;;; editor/init-editor.el -*- lexical-binding: t -*-

;; Require custom editor modules:
(require 'init-evil)
(require 'init-orgmode)
(require 'init-indentation)

;; Smarter lisp parents
(use-package smartparens
  :init
  (smartparens-global-mode +1))

(provide 'init-editor)
