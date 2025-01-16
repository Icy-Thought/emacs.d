;;; my-consult.el --- A Search & Navigation Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setopt register-preview-delay 0.5
          ;; Consult -> select xref locations with preview
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

;; :NOTE| Integrate consult with project.el

(use-package consult-project-extra
  :after (consult project))

(provide 'my-consult)
