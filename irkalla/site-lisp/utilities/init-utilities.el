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
    "h p" '(helpful-at-point :which-key "Show help for SYMB"))
  :config (setq-default help-window-select t))

(use-package screenshot
  :elpaca (:host github :repo "tecosaur/screenshot"))

;; :NOTE| Lastly, import our custom modules
(irkalla/enable-modules
 (direnv dired whichkey dashboard readers modeline consult embark vc vertico annotations recentf social terminal))

(provide 'init-utilities)
;;; init-utilities.el ends here
