;;; ui/init-doom-modeline.el -*- lexical-binding: t -*-

(defgroup irkalla-doom-modeline '()
  "a fancy and quick mode-line"
  :tag "Irkalla doom-modeline"
  :group 'irkalla)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-file-name 'relative-to-project)
  (doom-modeline-github t)
  (doom-modeline-github-interval (* 30 60))
  (doom-modeline-height 25)
  (if (display-graphic-p) (doom-modeline-hud t)))

(provide 'init-doom-modeline)
