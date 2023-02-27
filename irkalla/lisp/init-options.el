;;; init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Grouped (setq) settings
(setq
    auto-save-default t                            ; We don't like to lose unsaved work, do wo?
    display-line-numbers-type 'relative            ; Relative number-line
    truncate-string-ellipsis "↴"                   ; End lines with unicode rather than "..."
    scroll-margin 2)                               ; Quicker scrolling!

;; Grouped (setq-default) settings
(setq-default
    history-length 1000                            ; More = history -> better retention!
    prescient-history-length 1000                  ; For how long we retain that info.
    delete-by-moving-to-trash t                    ; Delete files to trash
    window-combination-resize t                    ; take new window space from all other windows (not just current)
    x-stretch-cursor t)                            ; Stretch cursor to the glyph width

(set-language-environment 'UTF-8)
(display-time-mode 1)
(global-subword-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(save-place-mode 1)

;; Change emacs default font-face
(set-face-attribute
  'default nil
  ':family "VictorMono Nerd Font"
  ':height 125
  ':weight 'semi-bold)

;; Adding a hint of of italics to our beloved font (FIXME!)
;; (custom-set-faces!
;;   '(font-lock-builtin-face :slant italic)
;;   '(font-lock-comment-face :slant italic)
;;   '(font-lock-function-name-face :weight bold :slane italic)
;;   '(font-lock-keyword-face :slant italic))

;; Frame -> Transparent (FIXME!)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(provide 'init-options)
