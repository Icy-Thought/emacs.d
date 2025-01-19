;;; my-helpul.el --- Guidance When Lost -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package helpful
  :bind
  ([remap describe-callable]    . helpful-callable)
  ([remap describe-function]    . helpful-function)
  ([remap describe-variable]    . helpful-variable)
  ([remap describe-key]         . helpful-key)
  ([remap view-emacs-debugging] . helpful-at-point)
  :init (setopt help-window-select t))

(use-feature which-key
  :hook (after-init . which-key-mode)
  :config (which-key-setup-minibuffer)
  :custom
  (which-key-allow-evil-operators t)
  (which-key-idle-delay 0.3)
  (which-key-show-remaining-keys t)
  (which-key-separator " → ")
  (which-key-sort-order 'which-key-prefix-then-key-order))

(provide 'my-helpul)
