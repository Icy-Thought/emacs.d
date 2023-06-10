;;; apparatus/init-eshell.el -*- lexical-binding: t -*-

(defgroup irkalla-eshell '()
  "A lisp-based Emacs shell environment. (+terminal)"
  :tag "Irkalla Eshell"
  :group 'irkalla)

(use-package eshell
  :elpaca nil
  :general
  (irkalla/space-lead-keydef
    "t t" '(eshell                  :which-key "Start Eshell")
    "t p" '(eshell                  :which-key "Start Eshell in project ROOT")
    "t c" '(eshell-life-is-too-much :which-key "Kill Eshell instance.."))
  (irkalla/comma-lead-keydef
    "t n" '(nix-eshell              :which-key "Create Nix (Eshell) environment")
    "t c" '(eshell-kill-process     :which-key "Kill running Eshell process"))
  :custom
  (eshell-tramp-initialize)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies t))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(provide 'init-eshell)
