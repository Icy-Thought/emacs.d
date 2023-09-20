;;; init-Diagnostics.el --- Language Diagnostics  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Diagnostics of our mistakes should become a priority, considering that we as humans are prone to making them.

;;; Code:

(use-package flymake
  :elpaca nil
  :hook (prog-mode . flymake-mode)
  :custom
  ;; (flymake-show-diagnostics-at-end-of-line t) <- way to much info..
  (flymake-fringe-indicator-position 'right-fringe)
  (elisp-flymake-byte-compile-load-path load-path))

(use-package flymake-collection
  :after flymake
  :hook (flymake-mode . flymake-collection-hook-setup))

;; :NOTE| Appending :flymake-hook to the keywords of use-package!
(elpaca-wait)

(with-eval-after-load "eglot"
 (add-to-list 'eglot-stay-out-of 'flymake)
 (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))

;; :NOTE| Minimal UI for LSP Diagnostics
(use-package sideline
  :custom
  (sideline-delay 0.2)
  (sideline-display-backend-name nil)
  (sideline-display-backend-type 'inner))

(use-package sideline-flymake
  :hook (flymake-mode  . sideline-mode)
  :custom (sideline-backends-right '((sideline-flymake  . down))))

(provide 'init-diagnostics)
;;; init-diagnostics.el ends here
