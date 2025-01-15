;;; typefaces.el --- Management Of Various Typefaces -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package fontaine
  :demand t
  :config
  (let ((default-font-family "VictorMono Nerd Font"))
    (setopt fontaine-presets
            `((regular)
              (reading
               :variable-pitch-family "Cardo"
               :variable-pitch-height 185
               :variable-pitch-slant normal
               :variable-pitch-weight regular)
              (presentation
               :default-height 175
               :default-weight medium)
              (t
               :default-family ,default-font-family
               :default-height 145
               :default-weight semibold

               :fixed-pitch-family ,default-font-family
               :fixed-pitch-height 145
               :fixed-pitch-slant normal

               :variable-pitch-family ,default-font-family
               :variable-pitch-height 1.00
               :variable-pitch-slant italic))))
  (fontaine-set-preset 'regular))

;; :NOTE| Assigning different font-faces for different languages

(use-feature face-remap
  :hook (text-mode . variable-pitch-mode)
  :bind (("C-0" . (lambda () (interactive) (text-scale-increase 0.0)))
         ("C-+" . (lambda () (interactive) (text-scale-increase 0.5)))
         ("C--" . (lambda () (interactive) (text-scale-decrease 0.5))))
  :config
  (set-fontset-font t 'arabic (font-spec :family "Scheherazade New") nil 'prepend)
  (set-fontset-font t 'han    (font-spec :family "Sarasa Mono CL")   nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

;; :NOTE| Customizing the slants of different faces

(use-feature font-lock
  :custom-face
  (font-lock-builtin-face       ((t (:slant italic))))
  (font-lock-comment-face       ((t (:slant italic))))
  (font-lock-doc-face           ((t (:slant italic))))
  (font-lock-function-name-face ((t (:slant italic :weight bold))))
  (font-lock-keyword-face       ((t (:slant italic))))
  (font-lock-preprocessor-face  ((t (:weight bold))))
  (font-lock-string-face        ((t (:slant italic))))
  :custom (font-lock-maximum-decoration t))

(provide 'irkalla/typefaces)
