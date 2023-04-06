;;; editor/init-evil.el -*- lexical-binding: t -*-

(defgroup irkalla-evil '()
  "adding our beloved vim bindings"
  :tag "Irkalla Evil"
  :group 'irkalla)

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-C-i-jump nil))              ; restore org-mode tab folding

(use-package evil-org
  :hook (org-mode-hook . evil-org-mode)
  :delight (evil-org-mode))

(use-package evil-collection
  :hook (evil-mode-hook . evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-escape
  :hook (evil-mode-hook . evil-escape-mode)
  :delight (evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unodered-key-sequence nil))

;; Highlight
(use-package evil-goggles
  :hook (evil-mode-hook . evil-goggles-mode)
  :config
  (setq evil-goggles-enable-delete nil
        evil-goggles-duration 0.100
        evil-goggles-async-duration 0.900)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-smartparens
  :after evil
  :hook (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide 'init-evil)
