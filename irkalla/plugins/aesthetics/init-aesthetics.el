;;; init-aesthetics.el -*- lexical-binding: t -*-

;; Require custom UI modules:
(require 'init-centaur-tabs)
(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-nerd-icons)
(require 'init-svg-tag)

;; :NOTE| Themes waiting to be used...
;; (require 'init-doom-themes)
;; (require 'init-ef-themes)

(defadvice load-theme (before clear-previous-themes activate)
  "Clear the way for our upcoming theme!"
  (mapc #'disable-theme custom-enabled-themes))

;; :NOTE| a well-made theming library for the ricer!
(use-package autothemer
  :config (load-theme 'rose-pine t))

;; Decoration: minor settings
(use-package emacs
  :elpaca nil
  :custom
  (truncate-lines t)
  (truncate-string-ellipsis "↴")
  (window-combination-resize t)
  (x-stretch-cursor t))

;; Notifications
(use-package alert
  :custom (alert-default-style 'libnotify))

;; Colorful parentheses
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
