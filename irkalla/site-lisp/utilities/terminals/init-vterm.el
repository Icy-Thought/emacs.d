;;; init-vterm.el --- Terminal Emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Reducing navigation between windows by providing Emcaas with a terminal eumlator.

;;; Code:

(use-package vterm
  :elpaca nil
  :hook (vterm-mode . evil-emacs-state)
  :bind (:map vterm-mode-map
              (:map evil-visual-state-map
                    ("<S-prior>" . #'scroll-down-command)
                    ("<S-next>"  . #'scroll-up-command)))
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(provide 'init-vterm)
;;; init-vterm.el ends here
