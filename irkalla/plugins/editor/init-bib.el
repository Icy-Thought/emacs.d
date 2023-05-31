;;; editor/init-bib.el -*- lexical-binding: t -*-

(defgroup irkalla-bib '()
  "An appropriate way to manage references/citations."
  :tag "Irkalla Bib"
  :group 'irkalla)

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :custom (citar-bibliography '("~/Notes/references.bib")))

(use-package citar-embark
  :after (citar embark)
  :hook (org-mode . citar-embark-mode)
  :config (setq citar-at-point-function 'embark-act))

(provide 'init-bib)
