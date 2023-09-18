;;; init-fontification.el --- Font-face Customizations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Modifying the default look of our fonts to achieve a better reading environment.

;;; Code:

(use-package faces
  :elpaca nil
  :custom-face
  (fixed-pitch    ((t (:family irkalla/default-font-family :height 125))))
  (variable-pitch ((t (:family irkalla/default-font-family :height 135)))))

(use-package face-remap
  :elpaca nil
  :bind (("C-0" . (lambda () (interactive) (text-scale-increase 0.0)))
         ("C-+" . (lambda () (interactive) (text-scale-increase 0.5)))
         ("C--" . (lambda () (interactive) (text-scale-decrease 0.5))))
  :config
  (set-fontset-font t 'arabic (font-spec :family "Scheherazade New") nil 'prepend)
  (set-fontset-font t 'han    (font-spec :family "Sarasa Mono CL")   nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

(use-package font-lock
  :elpaca nil
  :custom-face
  (font-lock-builtin-face       ((t (:slant italic))))
  (font-lock-comment-face       ((t (:slant italic))))
  (font-lock-doc-face           ((t (:slant italic))))
  (font-lock-function-name-face ((t (:slant italic :weight bold))))
  (font-lock-keyword-face       ((t (:slant italic))))
  (font-lock-preprocessor-face  ((t (:weight bold))))
  (font-lock-string-face        ((t (:slant italic)))))

(provide 'init-fontification)
;;; init-fontification.el ends here
