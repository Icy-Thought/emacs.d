;;; init-terminals.el --- Terminal Emulator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Reducing navigation between windows by providing Emcaas with a terminal eumlator.

;;; Code:

(use-package vterm
  :elpaca nil
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :general
  (:states 'insert :keymaps 'vterm-mode-map
   "<S-prior>" #'scroll-down-command
   "<S-next>"  #'scroll-up-command)

  (irkalla/space-lead-keydef
    "t"   '(:ignore t    :which-key "Terminal")
    "t t" '(vterm-toggle :which-key "Open VTerm"))
  
  (irkalla/comma-lead-keydef
    "t"   '(:ignore t       :which-key "Terminal")
    "t c" '(vterm-copy-mode :which-key "Terminal -> read-only"))

  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-use-dedicated-buffer t))

(provide 'init-terminals)
;;; init-terminals.el ends here
