;;; vterm.el --- Quick Terminal Emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature vterm
  :commands (vterm)
  :hook (vterm-mode . evil-emacs-state)
  :bind (:map vterm-mode-map
              ("<S-prior>" . #'scroll-down-command)
              ("<S-next>"  . #'scroll-up-command))
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(provide 'irkalla/vterm)
