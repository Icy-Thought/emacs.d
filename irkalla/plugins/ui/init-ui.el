;;; init-ui.el -*- lexical-binding: t -*-

;; Require custom ui modules:
(require 'init-centaur-tabs)
(require 'init-doom-modeline)
(require 'init-doom-themes)

;; Decorating with icons
(use-package all-the-icons
  :if (display-graphic-p))

(provide 'init-ui)
