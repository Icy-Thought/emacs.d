;;; init-doom-themes.el -*- lexical-binding: t -*-

(defgroup irkalla-doom-themes '()
    "a megapack of themes for GNU Emacs"
    :tag "Irkalla Doom-Themes"
    :group 'irkalla)

(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t))

(provide 'init-doom-themes)
