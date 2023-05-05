;;; ui/init-ef-themes.el -*- lexical-binding: t -*-

(defgroup irkalla-ef-themes '()
  "Colorful and legible themes for GNU Emacs."
  :tag "Irkalla Ef-Themes"
  :group 'irkalla)

(use-package ef-themes
  :init (load-theme (intern "ef-winter") t)
  :custom
  (ef-themes-select 'ef-winter)
  (ef-themes-to-toggle '(ef-summer ef-winter))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-region '(intense no-extend neutral)))

(provide 'init-ef-themes)
