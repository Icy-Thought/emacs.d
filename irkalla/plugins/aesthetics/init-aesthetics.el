;;; init-aesthetics.el -*- lexical-binding: t -*-

;; Require custom UI modules:
(require 'init-centaur-tabs)
(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-doom-themes)
;; (require 'init-ef-themes)
(require 'init-nerd-icons)
(require 'init-svg-tag)

;; Decoration: minor settings
(use-package emacs
  :elpaca nil
  :custom
  (truncate-lines t)
  (truncate-string-ellipsis "↴")
  (window-combination-resize t)
  (x-stretch-cursor t))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear the way for our upcoming theme!"
  (mapc #'disable-theme custom-enabled-themes))

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

(provide 'init-aesthetics)
