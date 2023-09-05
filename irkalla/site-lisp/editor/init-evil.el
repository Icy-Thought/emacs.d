;;; init-evil.el --- Evil-Mode: VIM Bindings in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Emacs navigation is a bit tedious for a seasoned VIM user, thus we ought to integrate EVIL with Emacs.

;;; Code:

(use-package evil
  :hook (elpaca-after-init . evil-mode)
  :general (:states 'normal
             "M-j" #'pixel-scroll-up
             "M-k" #'pixel-scroll-down)
  :config (evil-set-undo-system 'undo-redo)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t))

(use-package evil-collection
  :after evil
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-surround
  :after evil
  :hook ((prog-mode text-mode) . evil-surround-mode))

(use-package evil-escape
  :after evil
  :hook (evil-mode . evil-escape-mode)
  :delight (evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unodered-key-sequence nil))

(use-package evil-goggles
  :after evil
  :hook (evil-mode . evil-goggles-mode)
  :custom
  (evil-goggles-enable-delete nil)
  (evil-goggles-duration 0.100)
  (evil-goggles-async-duration 0.900)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :after evil
  :general
  (irkalla/space-lead-keydef
    ";" '(evilnc-comment-operator           :which-key "Un/Comment -> code-block"))
  (irkalla/comma-lead-keydef
    ";" '(evilnc-comment-or-uncomment-lines :which-key "Un/Comment -> line")))

(provide 'init-evil)
;;; init-evil.el ends here
