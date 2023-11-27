;;; init-apheleia.el --- Apheleia: Buffer Formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A quick buffer formatting package thaht requires speicfication for formatting action awareness.

;;; Code:

(use-package apheleia
  :diminish apheleia-mode
  :preface
  (defun irkalla/apheleia-disable-formatting ()
    "When triggered -> disable apheleia formatting on save."
    (interactive)
    (remove-hook 'before-save-hook 'eglot-format-buffer t)
    (apheleia-mode -1))
  :hook (prog-mode . apheleia-mode)
  :general
  (irkalla/comma-lead-keydef
    "l"   '(:ignore t                           :which-key "LSP")
    "l d" '(irkalla/apheleia-disable-formatting :which-key "Disable Auto-format")))

(provide 'init-apheleia)
;;; init-apheleia.el ends here
