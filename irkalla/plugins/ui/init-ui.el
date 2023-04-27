;;; init-ui.el -*- lexical-binding: t -*-

;; Require custom ui modules:
(require 'init-centaur-tabs)
;; (require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-doom-themes)
;; (require 'init-ef-themes)
(require 'init-svg-tag)

;; Decoration: minor settings
(use-package emacs
  :straight (:type built-in)
  :custom
  (truncate-lines t)
  (truncate-string-ellipsis "↴")
  (window-combination-resize t)
  (x-stretch-cursor t))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear the way for our upcoming theme!"
  (mapc #'disable-theme custom-enabled-themes))

;; Decorating with icons
(use-package nerd-icons
  :custom (nerd-icons-font-family "VictorMono Nerd Font"))

(use-package nerd-icons-dired
  :straight (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :straight (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

;; Theming library
(use-package autothemer)

;; Notifications
(use-package alert
  :custom (alert-default-style 'libnotify))

;; Colorful parantheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery nil
        show-paren-when-point-inside-paren nil)
  (show-paren-mode))

;; Create a darker variation of our themes
;; (use-package solaire-mode
;;   :after doom-themes
;;   :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
;;   :custom (solaire-global-mode +1))

(provide 'init-ui)
