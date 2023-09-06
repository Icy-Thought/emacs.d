;;; init-font-faces.el --- Font-face Customizations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Modifying the default look of our fonts to achieve a better reading environment.

;;; Code:

(use-package face-remap
  :elpaca nil
  :bind (("C-0" . (lambda () (interactive) (text-scale-increase 0.0)))
         ("C-+" . (lambda () (interactive) (text-scale-increase 0.5)))
         ("C--" . (lambda () (interactive) (text-scale-decrease 0.5)))))

(use-package faces
  :elpaca nil
  :config
  (custom-set-faces
    '(fixed-pitch    ((t (:family irkalla/default-font-family :height 125))))
    '(variable-pitch ((t (:family irkalla/default-font-family :height 135))))))

(use-package font-lock
  :elpaca nil
  :defer t
  :custom-face
  (font-lock-builtin-face       ((t (:slant italic))))
  (font-lock-comment-face       ((t (:slant italic))))
  (font-lock-doc-face           ((t (:slant italic))))
  (font-lock-function-name-face ((t (:slant italic :weight bold))))
  (font-lock-keyword-face       ((t (:slant italic))))
  (font-lock-preprocessor-face  ((t (:weight bold))))
  (font-lock-string-face        ((t (:slant italic)))))

(provide 'init-font-faces)
;;; init-font-faces.el ends here
