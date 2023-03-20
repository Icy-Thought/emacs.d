;;; ui/init-ef-themes.el -*- lexical-binding: t -*-

(defgroup irkalla-ef-themes '()
  "Colorful and legible themes for GNU Emacs."
  :tag "Irkalla Ef-Themes"
  :group 'irkalla)

(use-package ef-themes
  :custom
  ;; (ef-themes-select 'ef-dark)
  (ef-themes-load-random 'dark)
  (ef-themes-to-toggle '(ef-summer ef-dark))
  ;; (ef-themes-mixed-fonts t)
  ;; (ef-themes-variable-pitch-ui t))
  :config
  (mapc #'disable-theme custom-enabled-themes))

(provide 'init-ef-themes)
