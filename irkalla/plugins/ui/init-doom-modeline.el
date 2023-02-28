;;; ui/init-doom-modeline.el -*- lexical-binding: t -*-

(defgroup irkalla-doom-modeline '()
  "a fancy and quick mode-line"
  :tag "Irkalla doom-modeline"
  :group 'irkalla)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(provide 'init-doom-modeline)
