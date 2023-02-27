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

;; Frame -> Transparent (FIXME!)
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; if AC == True -> display battery in modeline
(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode 1))

;; PDF-Tools
(setq-default pdf-view-display-size 'fit-width)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(provide 'init-options)
