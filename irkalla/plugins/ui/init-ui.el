;;; init-ui.el -*- lexical-binding: t -*-

(use-package all-the-icons
  :if (display-graphic-p))

;; Require custom ui modules:
(require 'init-centaur-tabs)
(require 'init-doom-modeline)
(require 'init-doom-themes)

(provide 'init-ui)
