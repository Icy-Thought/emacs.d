;;; init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)

;; Orderless, alternative comp system
(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-completion)
