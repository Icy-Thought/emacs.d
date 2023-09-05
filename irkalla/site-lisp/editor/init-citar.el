;;; init-citar.el --- Citar: Bibliographical References -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Managing our references is very logical, thus a system ought to be set in place.

;;; Code:

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :custom (citar-bibliography '("~/Workspace/memorandum/references.bib")))

(use-package citar-embark
  :after (citar embark)
  :hook (org-mode . citar-embark-mode)
  :config (setq citar-at-point-function 'embark-act))

(provide 'init-citar)
;;; init-citar.el ends here
