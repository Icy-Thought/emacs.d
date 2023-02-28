;;; editor/init-evil.el -*- lexical-binding: t -*-

(defgroup irkalla-evil '()
  "adding our beloved vim bindings"
  :tag "Irkalla Evil"
  :group 'irkalla)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t)

  :config
  (evil-set-undo-system 'undo-redo)
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

(use-package evil-smartparens
  :after evil
  :hook (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide 'init-evil)
