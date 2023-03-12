;;; completion/init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq-default tab-always-indent 'complete
		tab-first-completion 'word-or-paren-or-punct))

;; Orderless: alternative comp system
(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-completion)
