;;; init-browser.el --- Browser-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Having a browser ready at hand for quick information lookup is not a bad thing.

;;; Code:

(use-feature eww
  :preface
  (defun auto-readable-wikipedia ()
    "Run `eww-readable' if the current buffer is a Wikipedia article."
    (when (and (eq major-mode 'eww-mode)
               (string-match-p "\\bwikipedia\\.org\\b" (eww-current-url)))
      (eww-readable)))
  :hook (eww-after-render . auto-readable-wikipedia))

(use-package shrface
  :after (shr)
  :hook ((shrface-mode . (lambda ()
                           (visual-line-mode)
                           (variable-pitch-mode)))
         (eww-after-render . shrface-mode)
         (nov-mode . (lambda ()
                       (setopt nov-shr-rendering-functions '((img . nov-render-img)
                                                             (title . nov-render-title)))
                       (setq nov-shr-rendering-functions
                             (append nov-shr-rendering-functions shr-external-rendering-functions))
                       (shrface-mode +1))))
  :custom
  (shrface-href-versatile t)
  (shrface-bullets-bullet-list (when (featurep 'org-modern) org-modern-star))
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings))

(use-package shr-tag-pre-highlight
  :after (shr)
  :hook (eww-after-render . (lambda ()
                              (require 'shr-tag-pre-highlight)
                              (add-to-list 'shr-external-rendering-functions
                                           '(pre . shr-tag-pre-highlight)))))

(provide 'init-browser)
;;; init-browser.el ends here
