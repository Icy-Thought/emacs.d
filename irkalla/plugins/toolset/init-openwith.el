;;; toolset/init-openwith.el -*- lexical-binding: t -*-

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav"
                       "mov" "flv" "ogm" "ogg" "mkv"))
                    "mpv" '(file))
              (list (openwith-make-extension-regexp
                     '("xbm" "pbm" "pgm" "ppm" "pnm" "png" "gif" "bmp" "tif" "jpeg"))
                    "feh" '(file))
              (list (openwith-make-extension-regexp
                     '("pdf" "epub" "djvu" "azw3"))
                    "zathura" '(file)))))

(provide 'init-openwith)
