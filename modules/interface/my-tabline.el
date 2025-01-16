;;; my-tabline.el --- A Bar For The Growing Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package centaur-tabs
  :bind (:map centaur-tabs-mode-map
              ("C-<prior>"   . #'centaur-tabs-backward-group)
              ("C-<next>"    . #'centaur-tabs-forward-group)
              ("M-<prior>"   . #'centaur-tabs-backward)
              ("M-<next>"    . #'centaur-tabs-forward)
              ("M-S-<prior>" . #'centaur-tabs-move-current-tab-to-left)
              ("M-S-<next>"  . #'centaur-tabs-move-current-tab-to-right))
  :hook (elpaca-after-init . centaur-tabs-mode)
  :config
  (setopt centaur-tabs-excluded-prefixes
          `(,@centaur-tabs-excluded-prefixes
            "*" " *" "consult-partial-preview" "Ement" "magit"))
  :custom
  (centaur-tabs-height 35)
  ;; (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-enable-ido-completion nil))

(provide 'my-tabline)
