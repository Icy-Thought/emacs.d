;;; apparatus/init-apparatus.el -*- lexical-binding: t -*-

;; Remaining toolset modules!
(require 'init-consult)
(require 'init-embark)
(require 'init-magit)
(require 'init-matrix)
(require 'init-reader)
(require 'init-treemacs)
(require 'init-terminal)
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
