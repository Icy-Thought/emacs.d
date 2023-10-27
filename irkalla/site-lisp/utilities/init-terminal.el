;;; init-terminal.el --- Terminal Emulator for Emacs -*- lexical-binding: t -*-

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
  :general
  (:states 'insert :keymaps 'vterm-mode-map
           "<S-prior>" #'scroll-down-command
           "<S-next>"  #'scroll-up-command)

  (irkalla/comma-lead-keydef
    "t"   '(:ignore t       :which-key "Terminal")
    "t c" '(vterm-copy-mode :which-key "Terminal -> read-only"))
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(use-package vterm-toggle
  :requires (vterm)
  :commands (vterm-toggle vterm-toggle-cd)
  :general
  (irkalla/space-lead-keydef
    "t"   '(:ignore t       :which-key "Terminal")
    "t t" '(vterm-toggle    :which-key "Open VTerm")
    "t c" '(vterm-toggle-cd :which-key "Open VTerm"))
  :custom (vterm-toggle-fullscreen-p t))

(provide 'init-terminal)
;;; init-terminal.el ends here
