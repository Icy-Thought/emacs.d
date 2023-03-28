;;; completion/init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)

;; Emacs-related
(use-package emacs
  :ensure nil
  :init
  (setq completions-detailed t
        completion-ignore-case t
        completion-category-overrides '((eglot (styles orderless)))
        tab-always-indent 'completion
        tab-first-completion 'word-or-paren-or-punct))

;; Orderless: alternative comp system
(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-completion)
