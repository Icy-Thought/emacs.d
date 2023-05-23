;;; completion/init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)

;; Emacs-related
(use-package emacs
  :elpaca nil
  :init
  (setq completions-detailed t
        completion-ignore-case t
        tab-always-indent 'completion
        tab-first-completion 'word-or-paren-or-punct))

;; Orderless: alternative comp system
(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles  . (orderless flex)))
                                        (eglot (styles . (orderless flex))))))

;; Eglot
(use-package eglot
  :elpaca nil
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:hoverProvider)))

(provide 'init-completion)
