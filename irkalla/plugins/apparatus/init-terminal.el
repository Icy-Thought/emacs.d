;;; apparatus/init-terminal.el -*- lexical-binding: t -*-

(defgroup irkalla-terminal '()
  "A terminal emulator for our Emacs environment."
  :tag "Irkalla Terminal"
  :group 'irkalla)

(use-package vterm
  :elpaca nil
  :general
  (general-imap
    :keymaps 'vterm-mode-map
    "<S-prior>" #'scroll-down-command
    "<S-next>" #'scroll-up-command)
  (irkalla/space-lead-keydef "t t" '(vterm-toggle    :which-key "Launch Terminal Emulator (VTerm)"))
  (irkalla/comma-lead-keydef "t c" '(vterm-copy-mode :which-key "Read-only terminal -> copy, search etc."))
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-use-dedicated-buffer t))

(provide 'init-terminal)
