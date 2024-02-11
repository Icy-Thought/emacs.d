;;; init-ediff.el --- Ediff: Differentiate Versions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Files do change over time, and being able to differentiate the change is trivial.

;;; Code:

(use-package ediff
  :ensure nil
  :hook((ediff-prepare-buffer . outline-show-all)
        (ediff-quit . winner-undo))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(provide 'init-ediff)
;;; init-ediff.el ends here
