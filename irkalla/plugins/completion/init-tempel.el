;;; init-tempel.el -*- lexical-binding: t -*-

(defgroup irkalla-tempel '()
    "Simple templates for Emacs."
    :tag "Irkalla Tempel"
    :group 'irkalla)

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :bind (("M-+" . tempel-complete)
	 ("M-*" . tempel-insert)))

;; Adding existing tempel collection of snippets
(use-package tempel-collection
  :after tempel)

;; FIXME: migrate cdlatex -> tempel
(use-package cdlatex
  :init
  (add-hook 'latex-mode-hook #'turn-on-cdlatex))

(provide 'init-tempel)
