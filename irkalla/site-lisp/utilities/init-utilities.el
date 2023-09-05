;;; init-utilities.el --- Utilities-related Changes -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Tools that could improve our workflow are always welcomed!

;;; Code:

(use-package alert
  :custom (alert-default-style 'libnotify))

(use-package helpful)

(use-package openwith
  :hook (elpaca-after-init . openwith-mode)
  :config
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp
                     '("mpg" "mpeg" "mp3" "mp4" "avi" "wmv" "wav"
                       "mov" "flv" "ogm" "ogg" "mkv"))
                    "mpv" '(file)))))

(use-package emacs-everywhere
  :custom (emacs-everywhere-copy-command (list "cat" "%f" "|" "cb" "copy")))

(use-package screenshot
  :elpaca (:host github :repo "tecosaur/screenshot")
  :defer t)

(provide 'init-utilities)
;;; init-utilities.el ends here
