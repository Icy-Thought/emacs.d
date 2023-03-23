;;; ui/init-doom-themes.el -*- lexical-binding: t -*-

(defgroup irkalla-doom-themes '()
  "a megapack of themes for GNU Emacs"
  :tag "Irkalla Doom-Themes"
  :group 'irkalla)

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-treemacs-theme "doom-colors")

  (with-eval-after-load 'doom-themes
    (doom-themes-treemacs-config))

  (load-theme (intern "doom-tokyo-night") t))

(provide 'init-doom-themes)
