;;; init-ui.el -*- lexical-binding: t -*-

;; Require custom ui modules:
(require 'init-centaur-tabs)
(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-doom-themes)
;; (require 'init-ef-themes)
;; (require 'init-solaire)
(require 'init-svg-tag)

;; Decoration: minor settings
(use-package emacs
  :ensure nil
  :custom
  (truncate-lines t)
  (truncate-string-ellipsis "↴")
  (window-combination-resize t)
  (x-stretch-cursor t))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear the way for our upcoming theme!"
  (mapc #'disable-theme custom-enabled-themes))

;; Decorating with icons
(use-package all-the-icons)

;; Notifications
(use-package alert
  :custom
  (alert-default-style 'libnotify))

;; Colorful parantheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery nil
        show-paren-when-point-inside-paren nil)
  (show-paren-mode))

(provide 'init-ui)
