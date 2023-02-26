;;; init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Grouped settings:
(setq
    auto-save-default t                            ; We don't like to lose unsaved work, do wo?
    display-line-numbers-type 'relative            ; Relative number-line
    truncate-string-ellipsis "↴"                   ; End lines with unicode rather than "..."
    scroll-margin 2)                               ; Quicker scrolling!

;; Grouped defaults:
(setq-default
    history-length 1000                            ; More = history -> better retention!
    prescient-history-length 1000                  ; For how long we retain that info.
    delete-by-moving-to-trash t                    ; Delete files to trash
    window-combination-resize t                    ; take new window space from all other windows (not just current)
    x-stretch-cursor t)                            ; Stretch cursor to the glyph width

;; Font Face
(set-face-attribute
  'default nil
  ':font "VictorMono Nerd Font"
  ':height 125
  ':weight 'semi-bold)

;; Set Emacs default encoding
(set-language-environment 'UTF-8)

;; Enable mode-line timer
(display-time-mode 1)

;; Iterate through CamelCase:
(global-subword-mode 1)

;; <ESC> -> cancel all/everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Jump back to last edit location on buf-enter
(save-place-mode 1)

(provide 'init-options)
