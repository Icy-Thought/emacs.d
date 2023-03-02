;;; lisp/init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Grouped (setq) settings
(setq
 auto-save-default t                            ; We don't like to lose unsaved work, do wo?
 display-line-numbers-type 'relative            ; Relative number-line
 truncate-string-ellipsis "↴"                   ; End lines with unicode rather than "..."
 visual-bell nil)                    ; Disable modeline (red) flashes

;; Grouped (setq-default) settings
(setq-default
 display-line-number-mode t
 history-length 1000
 prescient-history-length 1000
 delete-by-moving-to-trash t
 indent-tabs-mode nil
 tab-always-indent t
 tab-first-completion 'word-or-paren-or-punct
 truncate-lines t
 window-combination-resize t
 x-stretch-cursor t)                            ; Stretch cursor to the glyph width

(global-display-line-numbers-mode)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-subword-mode 1)
(save-place-mode 1)
(set-language-environment 'UTF-8)

;; Smooth sccrolling (Emacs >= 29)
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Frame -> Transparent (FIXME!)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; PDF-Tools
(setq-default pdf-view-display-size 'fit-width)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(provide 'init-options)
