;;; windows.el --- Control of Emacs Windows -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Window navigation ought to be a simple task!

(use-feature windmove
  :config
  (windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; :NOTE| Toggle focus/unfocus of certain buffers on demand

(use-feature winner
  :hook (elpaca-after-init . winner-mode))

;; :NOTE| Clear dividers for split buffers

(use-feature frame
  :hook (before-make-frame . window-divider-mode)
  :custom
  (window-divider-default-places t)
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2))

(provide 'irkalla/windows)
