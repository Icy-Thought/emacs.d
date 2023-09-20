;;; init-decorations.el --- Decoration-related Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; We cannot allow Emacs to co-exist with aesthetically modified applications without applying some form of pleasantaries to it!

;;; Code:

(irkalla/enable-modules
 (icons svg-tags))

(use-package emacs
  :elpaca nil
  :preface
  (defun irkalla/toggle-frame-transparency ()
    "Toggle (on/off) Emacs frame transparency on demand!"
    (interactive)
    (let ((alpha-value
           (if (equal (frame-parameter nil 'alpha-background) 100)
               85 100)))
      (set-frame-parameter nil 'alpha-background alpha-value)
      (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))))
  :general
  (irkalla/comma-lead-keydef
    "q a" '(irkalla/toggle-frame-transparency :which-key "Toggle Opacity")))  

(use-package autothemer
  :demand t
  :preface
  (defun irkalla/apply-theme (palette)
    "A quicker way to apply our installed themes."
    (if (custom-theme-p palette)
        (enable-theme palette)
      (load-theme palette :no-confirm)))
  :config (irkalla/apply-theme 'kanagawa))

;; :NOTE| Replace several symbols with prettier alternatives
(use-package prettify-symbols
  :elpaca nil
  :hook (emacs-lisp-mode . prettify-symbols-mode)
  :custom (prettify-symbols-unprettify-at-point 'right-edge))

(provide 'init-decorations)
;;; init-decorations.el ends here
