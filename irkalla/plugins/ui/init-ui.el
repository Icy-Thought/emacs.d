;;; init-ui.el -*- lexical-binding: t -*-

;; Require custom ui modules:
(require 'init-centaur-tabs)
(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-doom-themes)
;; (require 'init-solaire)

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

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 25)
   (internal-border-width . 25)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'init-ui)
