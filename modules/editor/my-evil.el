;;; my-evil.el --- VIM Based Navigation Inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package evil
  :preface
  (defun irkalla/extended-escape ()
    (interactive)
    (if (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
      (evil-force-normal-state)))
  :bind (:map evil-normal-state-map ("<escape>" . irkalla/extended-escape))
  :hook (elpaca-after-init . evil-mode)
  :config (evil-select-search-module 'evil-search-module 'evil-search)
  :custom
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-fu)
  (evil-vsplit-window-right t))

;; :NOTE| A collection of Evil bindings

(use-package evil-collection
  :after (evil)
  :hook (evil-mode . evil-collection-init)
  :config
  (with-eval-after-load 'corfu
    (setopt evil-collection-corfu-key-themes '(tab-n-go))
    (advice-add 'corfu--setup :after
                (lambda (&rest _) (setopt corfu-preselect 'valid))))
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-magit-use-y-for-yank t)
  (evil-collection-magit-want-horizontal-movement t))

;; :NOTE| 2 character word search

(use-package evil-snipe
  :after (evil)
  :hook (((prog-mode text-mode) . evil-snipe-local-mode)
         (evil-snipe-local-mode . evil-snipe-override-local-mode))
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-repeat-scope 'whole-visible)
  (evil-snipe-spillover-scope nil)
  :config (push '(?\[ "[[{(]") evil-snipe-aliases))

;; :NOTE| Encapsulate words with $SYMB

(use-package evil-surround
  :after (evil)
  :hook (evil-mode . global-evil-surround-mode))

;; :NOTE| Animate certain Evil actions

(use-package evil-goggles
  :after (evil)
  :hook (evil-mode . evil-goggles-mode)
  :custom (evil-goggles-duration 0.1))

;; :NOTE| Evil-based word/line/block commenting

(use-package evil-nerd-commenter :after (evil))

;; :NOTE| Edit with the help of multiple cursors!

(use-package evil-multiedit
  :after (evil)
  :config (evil-multiedit-default-keybinds))

(provide 'my-evil)
