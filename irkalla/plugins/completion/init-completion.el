;;; completion/init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)
(require 'init-coding)
(require 'init-babel)
(require 'init-chatgpt)

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

;; :NOTE| configuring our LSP servers
(use-package eglot
  :elpaca nil
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:hoverProvider)))

(use-package flymake
  :elpaca nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-fringe-indicator-position 'right-fringe)
  (elisp-flymake-byte-compile-load-path load-path))

(provide 'init-completion)
