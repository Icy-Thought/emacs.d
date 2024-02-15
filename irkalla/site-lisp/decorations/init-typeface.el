;;; init-typeface.el --- Typeface Customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Modifying the our typeface to achieve a better reading environment is necessary!

;;; Code:

(use-package fontaine
  :demand t
  :preface
  (defvar irkalla/default-font     "VictorMono Nerd Font")
  (defvar irkalla/fixed-pitch-font "VictorMono Nerd Font Mono")
  :hook (kill-emacs-hook . fontaine-store-latest-preset)
  :custom
  (fontaine-presets `((regular) ;; fallback values
                      (large
                       :default-weight semibold
                       :default-height 180
                       :bold-weight extrabold)
                      (t
                       :default-family ,irkalla/default-font
                       :default-weight semibold
                       :default-height 145
                       :fixed-pitch-family ,irkalla/fixed-pitch-font
                       :fixed-pitch-height nil
                       :variable-pitch-family nil
                       :variable-pitch-height 1.05
                       :bold-weight bold
                       :italic-weight italic)))
  :config (fontaine-set-preset 'regular))

(use-feature face-remap
  :bind (("C-0" . (lambda () (interactive) (text-scale-increase 0.0)))
         ("C-+" . (lambda () (interactive) (text-scale-increase 0.5)))
         ("C--" . (lambda () (interactive) (text-scale-decrease 0.5))))
  :config
  (set-fontset-font t 'arabic (font-spec :family "Scheherazade New") nil 'prepend)
  (set-fontset-font t 'han    (font-spec :family "Sarasa Mono CL")   nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

(use-feature font-lock
  :custom-face
  (font-lock-builtin-face       ((t (:slant italic))))
  (font-lock-comment-face       ((t (:slant italic))))
  (font-lock-doc-face           ((t (:slant italic))))
  (font-lock-function-name-face ((t (:slant italic :weight bold))))
  (font-lock-keyword-face       ((t (:slant italic))))
  (font-lock-preprocessor-face  ((t (:weight bold))))
  (font-lock-string-face        ((t (:slant italic)))))

(provide 'init-typeface)
;;; init-typeface.el ends here
