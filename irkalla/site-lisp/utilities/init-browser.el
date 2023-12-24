;;; init-browser.el --- Browser-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Having a browser ready at hand for quick information lookup is not a bad thing.

;;; Code:

(use-package eww
  :elpaca nil
  :preface
  (defun auto-readable-wikipedia ()
    "Run `eww-readable' if the current buffer is a Wikipedia article."
    (when (and (eq major-mode 'eww-mode)
               (string-match-p "\\bwikipedia\\.org\\b" (eww-current-url)))
      (eww-readable)))
  :hook (eww-after-render . auto-readable-wikipedia))

(use-package shrface
  :requires (shr)
  :hook ((shrface-mode . olivetti-mode)
         (eww-after-render . shrface-mode))
  :custom
  (shrface-href-versatile t)
  (shrface-bullets-bullet-list (when (featurep 'org-modern) org-modern-star))
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)

  (when (featurep 'nov)
    (add-hook 'nov-mode-hook #'shrface-mode)
    (setq nov-shr-rendering-functions '((img . nov-render-img)
                                        (title . nov-render-title)))
    (setq nov-shr-rendering-functions
          (append nov-shr-rendering-functions shr-external-rendering-functions))))

(use-package shr-tag-pre-highlight
  :requires (shr)
  :config (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(provide 'init-browser)
;;; init-browser.el ends here
