;;; my-browser.el --- Navigate The Internet Through Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature eww
  :preface
  (defun auto-readable-wikipedia ()
    "Run `eww-readable' if the current buffer is a Wikipedia article."
    (when (and (eq major-mode 'eww-mode)
               (string-match-p "\\bwikipedia\\.org\\b" (eww-current-url)))
      (eww-readable)))
  :hook (eww-after-render . auto-readable-wikipedia))

(provide 'my-browser)
