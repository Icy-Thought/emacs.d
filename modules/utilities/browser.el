;;; browser.el --- Navigate The Internet Through Emacs -*- lexical-binding: t; -*-

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


;; :NOTE| Render webpages similar to Org-Modern

(use-package shrface
  :hook ((shrface-mode . variable-pitch-mode)
         (nov-mode . shrface-mode)
         (eww-after-render . shrface-mode))
  :custom (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)

  (with-eval-after-load 'org-modern
    (setopt shrface-bullets-bullet-list
            (string-glyph-split org-modern-replace-stars))))

;; :NOTE| Highlight code-blocks in webpages

(use-package shr-tag-pre-highlight
  :after (shrface)
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight))
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))

(provide 'irkalla/browser)
