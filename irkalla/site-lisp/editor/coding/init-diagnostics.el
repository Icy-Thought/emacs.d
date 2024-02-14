;;; init-diagnostics.el --- On-The-Fly Syntax Checking -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Diagnostics of our mistakes should become a priority, considering that we as humans are prone to making them.

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (elisp-flymake-byte-compile-load-path load-path))

;; :NOTE| Change of heart for our bitmaps
;; (flymake-error-bitmap   '("" compilation-error))
;; (flymake-note-bitmap    '("" compilation-info))
;; (flymake-warning-bitmap '("" compilation-warning)))

;; :NOTE| Minimal UI for LSP Diagnostics
(use-package sideline
  :commands (sideline-mode)
  :custom
  (sideline-delay 0.2)
  (sideline-display-backend-name nil)
  (sideline-display-backend-type 'inner))

(use-package sideline-flymake
  :requires (sideline)
  :hook (flymake-mode  . sideline-mode)
  :custom (sideline-backends-right '((sideline-flymake  . down))))

(provide 'init-diagnostics)
;;; init-diagnostics.el ends here
