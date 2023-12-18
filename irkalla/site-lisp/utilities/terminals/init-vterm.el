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
  :general
  (:states 'insert :keymaps 'vterm-mode-map
           "<S-prior>" #'scroll-down-command
           "<S-next>"  #'scroll-up-command)

  (irkalla/comma-lead-keydef
    "t"   '(:ignore t              :which-key "Terminal")
    "t y" '(vterm-copy-mode        :which-key "Copy contents from VTerm"))
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t))

(use-package vterm-toggle
  :requires (vterm)
  :commands (vterm-toggle vterm-toggle-cd)
  :general
  (irkalla/space-lead-keydef
    "t"   '(:ignore t              :which-key "Terminal")
    "t t" '(vterm-toggle           :which-key "Toggle Terminal"))
  (irkalla/comma-lead-keydef
    "t c" '(vterm-toggle-insert-cd :which-key "cd project-dir"))
  :custom (vterm-toggle-fullscreen-p t))

(use-package multi-vterm
  :requires (vterm)
  :commands (multi-vterm)
  :general
  (irkalla/space-lead-keydef
    "t n" '(multi-vterm                  :which-key "New Terminal")
    "t j" '(multi-vterm-next             :which-key "Next")
    "t k" '(multi-vterm-prev             :which-key "Previous")
    "t d" '(multi-vterm-dedicated-toggle :which-key "Dedicated Terminal")
    "t r" '(multi-vterm-projectile       :which-key "Projectile Terminal"))
  :custom (multi-vterm-buffer-name "VTerm"))

(provide 'init-vterm)
;;; init-vterm.el ends here
