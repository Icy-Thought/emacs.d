;;; ui/init-solaire.el -*- lexical-binding: t -*-

(use-package solaire-mode
  :after doom-themes
  :config (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  :custom (solaire-global-mode +1))

(provide 'init-solaire)
