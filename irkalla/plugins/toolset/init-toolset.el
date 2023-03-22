;;; toolset/init-toolset.el -*- lexical-binding: t -*-

;; Require custom toolset modules:
(require 'init-consult)
(require 'init-elfeed)
(require 'init-embark)
;; (require 'init-hydra)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-treemacs)
(require 'init-vertico)
(require 'init-whichkey)

;; PDF-Tools: Darker + Width
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :config (pdf-loader-install)
  :custom (pdf-view-display-size 'fit-width))
;; (pdf-view-midnight-colors '("#ffffff" . "#000000")))


;; OpenWith: better alternative to Emacs
(use-package openwith
  :init (openwith-mode t)
  :config
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav"
                       "mov" "flv" "ogm" "ogg" "mkv"))
                    "mpv" '(file)))))

(provide 'init-toolset)
