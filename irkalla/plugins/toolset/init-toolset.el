;;; toolset/init-toolset.el -*- lexical-binding: t -*-

;; Require custom toolset modules:
(require 'init-consult)
(require 'init-embark)
(require 'init-hydra)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-treemacs)
(require 'init-vertico)
(require 'init-whichkey)

;; PDF-Tools: Darker + Width
(use-package pdf-tools
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (setq-default pdf-view-display-size 'fit-width))

(provide 'init-toolset)
