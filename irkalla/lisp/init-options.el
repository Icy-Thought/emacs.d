;;; lisp/init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Grouped (setq) settings
(setq
 auto-save-default t                            ; We don't like to lose unsaved work, do wo?
 ring-bell-function #'ignore                    ; Annoying flashes, begone!!
 visible-bell nil
 next-error-message-highlight t                 ; Highlight error messages in next buffer
 history-delete-duplicates t)

;; Grouped (setq-default) settings
(setq-default
 display-line-number-mode t
 display-line-numbers-type 'relative)

;; History: increased!
(setq-default
 history-length 1000
 prescient-history-length 1000
 delete-by-moving-to-trash t)

;; Completion: minor settings
(setq-default
 tab-always-indent t
 tab-first-completion 'word-or-paren-or-punct)

;; Decoration: minor settings
(setq-default
 truncate-lines t
 truncate-string-ellipsis "↴"
 window-combination-resize t
 x-stretch-cursor t)

(global-display-line-numbers-mode)

;; Indentation: 2 -> 4
(setq-default
 indent-tabs-mode nil                   ; tabs -> spaces
 electric-indent-inhibit t
 standard-indent 4
 tab-width 4)

;; Logical changes.. (🫠)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-subword-mode 1)
(save-place-mode 1)
(set-language-environment 'UTF-8)

;; Smooth scrolling (Emacs >= 29)
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Frame -> Transparent (FIXME!)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(provide 'init-options)
