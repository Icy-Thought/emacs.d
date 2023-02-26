;;; init-evil.el -*- lexical-binding: t -*-

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

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(provide 'init-evil)
