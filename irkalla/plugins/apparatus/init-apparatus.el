;;; toolset/init-apparatus.el -*- lexical-binding: t -*-

;; Important modules first!
(require 'init-hydra)
(require 'init-whichkey)

;; Remaining toolset modules!
(require 'init-consult)
(require 'init-elfeed)
(require 'init-embark)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-treemacs)
(require 'init-vertico)

;; PDF-Tools: Darker + Width
(use-package pdf-tools
  :straight (:type built-in)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :custom
  (pdf-loader-install)
  (pdf-view-display-size 'fit-width))

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (expand-file-name "pdf-view-restore" user-emacs-cache-directory)))

;; EPUB Reader!
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :magic ("%EPUB" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode))
  :custom
  (nov-text-width t)
  (nov-text-width 120))

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
