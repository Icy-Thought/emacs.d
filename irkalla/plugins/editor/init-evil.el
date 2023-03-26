;;; editor/init-evil.el -*- lexical-binding: t -*-

(defgroup irkalla-evil '()
  "adding our beloved vim bindings"
  :tag "Irkalla Evil"
  :group 'irkalla)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-tree)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-C-i-jump nil)              ; restore org-mode tab folding
  :config
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'messages-buffer-mode 'normal)

  ;; Finally, call-forward our Evil doings!
  (evil-mode 1))

(use-package evil-org
  :after org
  :delight (evil-org-mode)
  :hook (org-mode-hook . evil-org-mode))

(use-package evil-collection
  :after evil
  :hook (evil-mode-hook . evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-escape
  :after evil
  :delight (evil-escape-mode)
  :hook (evil-mode-hook . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unodered-key-sequence nil))

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

(provide 'init-evil)
