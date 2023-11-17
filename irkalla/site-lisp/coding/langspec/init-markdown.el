;;; init-haskell.el --- Markdown -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A widely used and very minimalistic documentation format.

;;; Code:

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :general
  (irkalla/comma-lead-keydef markdown-mode-map
    "m d"    '(markdown-do :which-key "Perform -> action"))
  :custom-face
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.25 :weight extra-bold))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.15 :weight bold))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.08 :weight bold))))
  (markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.00 :weight bold))))
  (markdown-header-face-5 ((t (:inherit markdown-header-face :height 0.90 :weight bold))))
  (markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.75 :weight extra-bold))))
  :custom (markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
