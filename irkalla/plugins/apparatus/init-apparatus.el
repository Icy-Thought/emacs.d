;;; toolset/init-apparatus.el -*- lexical-binding: t -*-

;; Important modules first!
(require 'init-hydra)
(require 'init-whichkey)

;; Remaining toolset modules!
(require 'init-consult)
(require 'init-embark)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-reader)
(require 'init-treemacs)
(require 'init-vertico)

;; OpenWith: better alternative to Emacs
(use-package openwith
  :init (openwith-mode t)
  :config
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav"
                       "mov" "flv" "ogm" "ogg" "mkv"))
                    "mpv" '(file)))))

(provide 'init-apparatus)
