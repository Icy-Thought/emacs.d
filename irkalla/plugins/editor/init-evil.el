;;; editor/init-evil.el -*- lexical-binding: t -*-

(defgroup irkalla-evil '()
  "adding our beloved vim bindings"
  :tag "Irkalla Evil"
  :group 'irkalla)

(use-package evil
  :hook (elpaca-after-init . evil-mode)
  :general (:keymaps 'evil-normal-state-map
                     "M-j" #'pixel-scroll-up
                     "M-k" #'pixel-scroll-down)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-C-i-jump nil)              ; restore org-mode tab folding
  (evil-set-initial-state 'dashboard-mode 'emacs))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :delight (evil-org-mode))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :delight (evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unodered-key-sequence nil))

;; Highlight
(use-package evil-goggles
  :hook (evil-mode . evil-goggles-mode)
  :init (setq evil-goggles-enable-delete nil)
  :custom
  (evil-goggles-duration 0.100)
  (evil-goggles-async-duration 0.900)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :after evil
  :general
  (irkalla/comma-lead-keydef ";" '(evilnc-comment-or-uncomment-lines :which-key "Comment/uncomment selected line"))
  (irkalla/space-lead-keydef ";" '(evilnc-comment-operator           :which-key "Comment code-block at cursor")))

(provide 'init-evil)
