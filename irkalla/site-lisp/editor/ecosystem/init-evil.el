;;; init-evil.el --- Evil-Mode: VIM Bindings in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Emacs navigation is a bit tedious for a seasoned VIM user, thus we ought to integrate EVIL with Emacs.

;;; Code:

(use-package evil
  :hook (elpaca-after-init . evil-mode)
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
  :custom (evil-goggles-duration 0.1))

(use-package evil-nerd-commenter
  :requires (evil)
  :commands (evilnc-comment-or-uncomment-lines
             evilnc-comment-or-uncomment-paragraphs))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ editor-hydra ()
    ("Action"
     ((";" evilnc-comment-or-uncomment-lines      "Comment line"))))

  (pretty-hydra-define+ visual-editor-hydra ()
    ("Action"
     ((";" evilnc-comment-or-uncomment-lines "Comment Line(s)")))))

(provide 'init-evil)
;;; init-evil.el ends here
