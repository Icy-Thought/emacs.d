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
  :config (evil-set-undo-system 'undo-fu)
  :custom
  (evil-respect-visual-line-mode t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-integration t)
  (evil-want-keybinding nil))

(use-package evil-collection
  :requires (evil)
  :hook (evil-mode . (lambda ()
                       (evil-collection-init)
                       (setopt evil-want-keybinding t)))
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-snipe
  :requires (evil)
  :hook (((prog-mode text-mode) . evil-snipe-local-mode)
         (evil-snipe-local-mode . evil-snipe-override-local-mode))
  :general (:states 'visual :keymaps 'evil-snipe-local-mode-map
                    "z" #'evil-snipe-s
                    "Z" #'evil-snipe-S)
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-scope 'whole-visible)
  (evil-snipe-spillover-scope nil)
  :config (push '(?\[ "[[{(]") evil-snipe-aliases))

(use-package evil-surround
  :requires (evil)
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-escape
  :requires (evil)
  :delight (evil-escape-mode)
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  (evil-escape-unodered-key-sequence nil))

(use-package evil-goggles
  :requires (evil)
  :hook (evil-mode . evil-goggles-mode)
  :custom
  (evil-goggles-enable-delete nil)
  (evil-goggles-duration 0.100)
  (evil-goggles-async-duration 0.900)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
  :requires (evil)
  :general
  (irkalla/space-lead-keydef
    ";" '(evilnc-comment-operator           :which-key "Un/Comment -> code-block"))
  (irkalla/comma-lead-keydef
    ";" '(evilnc-comment-or-uncomment-lines :which-key "Un/Comment -> line")))

(provide 'init-evil)
;;; init-evil.el ends here
