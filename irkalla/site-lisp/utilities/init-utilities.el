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

(use-package emacs-everywhere
  :custom (emacs-everywhere-copy-command (list "cat" "%f" "|" "cb" "copy")))

(use-package helpful
  :general
  (irkalla/space-lead-keydef
    "h"   '(:ignore t        :which-key "Helpful")
    "h k" '(helpful-key      :which-key "Key")
    "h f" '(helpful-callable :which-key "Function")
    "h v" '(helpful-variable :which-key "Variable")
    "h C" '(helpful-command  :which-key "Command")
    "h F" '(helpful-function :which-key "Interactive functions"))

  (irkalla/comma-lead-keydef emacs-lisp-mode-map
    "h"   '(:ignore t        :which-key "Helpful")
    "h p" '(helpful-at-point :which-key "Show help for SYMB")))

(use-package openwith
  :hook (elpaca-after-init . openwith-mode)
  :config
  (setq openwith-associations
        `((,(openwith-make-extension-regexp
             '("mpg" "mpeg" "mp3" "mp4"
               "avi" "wmv" "wav" "mov" "flv"
               "ogm" "ogg" "mkv"))
           "mpv" '(file))
          (,(openwith-make-extension-regexp
             '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
           "libreoffice" (file)))))

(use-package screenshot
  :elpaca (:host github :repo "tecosaur/screenshot"))

;; :NOTE| Lastly, import our custom modules
(irkalla/enable-modules
 (direnv whichkey dashboard readers centaur modeline consult embark vc vertico annotations recentf benchmark social
  terminals))

(provide 'init-utilities)
;;; init-utilities.el ends here
