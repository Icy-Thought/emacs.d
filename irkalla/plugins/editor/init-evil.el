;;; editor/init-evil.el -*- lexical-binding: t -*-

(defgroup irkalla-evil '()
  "adding our beloved vim bindings"
  :tag "Irkalla Evil"
  :group 'irkalla)

(use-package evil
  :custom
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Bindings
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Highlight
(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-enable-delete nil
	evil-goggles-duration 0.100
	evil-goggles-async-duration 0.900)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-smartparens
  :after evil
  :hook (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package undo-fu
  :demand t
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.
  )

(provide 'init-evil)
