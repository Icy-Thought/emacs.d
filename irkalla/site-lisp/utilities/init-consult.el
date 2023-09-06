;;; init-consult.el --- Consult: Consulting completing-read -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Better UI for our Emacs completion related tasks.

;;; Code:

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Consult -> select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (consult-narrow-key "<") ;; "C-+"

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package consult-projectile
  :after projectile
  :general
  (irkalla/space-lead-keydef
    "p p" '(consult-projectile-switch-project :which-key "Switch Project")))

(provide 'init-consult)
;;; init-consult.el ends here
