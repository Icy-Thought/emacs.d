;;; ui/init-solaire.el -*- lexical-binding: t -*-

(defgroup irkalla-solaire '()
  "Give our themes a bit of a tan."
  :tag "Irkalla Solaire-Mode"
  :group 'irkalla)

(use-package solaire-mode
  :after doom-themes
  :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  :custom (solaire-global-mode +1))

(provide 'init-solaire)
