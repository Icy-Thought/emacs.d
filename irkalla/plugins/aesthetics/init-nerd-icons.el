;;; init-nerd-icons.el -*- lexical-binding: t -*-

(defgroup irkalla-nerd-icons '()
  "a library for easily using Nerd Font icons inside Emacs"
  :tag "Irkalla Nerd Icons"
  :group 'irkalla)

;; Decorating with icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family irkalla/default-font)
  (nerd-icons-scale-factors 1.25))

(use-package nerd-icons-dired
  :elpaca (nerd-icons-dired :host github :repo "rainstormstudio/nerd-icons-dired")
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :elpaca (nerd-icons-completion :host github :repo "rainstormstudio/nerd-icons-completion")
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

(use-package treemacs-nerd-icons
  :elpaca (treemacs-nerd-icons :host github :repo "rainstormstudio/treemacs-nerd-icons")
  :after (nerd-icons treemacs )
  :config (treemacs-load-theme "nerd-icons"))

(provide 'init-nerd-icons)
