;;; init-tempel.el --- Tempel: Snippet Management -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Reducing boilerplate completions into snippets is never a wrong idea!

;;; Code:

(use-package tempel
  :preface
  (defun tempel-setup-capf ()
    "Integrating Temple with Corfu to automatically display possible snippet completions."
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))
  :hook ((conf-mode prog-mode text-mode) . tempel-setup-capf)
  :custom (tempel-path (concat irkalla/completion-dir "/snippets/*.eld")))

;; Adding pre-defined bindings to our stack
(use-package tempel-collection
  :requires (tempel))

;; :TODO| CDLaTeX Alternative

(provide 'init-tempel)
;;; init-tempel.el ends here
