;;; init-frames.el --- Manipulation of Emacs Frames -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Controlling the Frames of our Emacs instance, zooming in and whatnot.

;;; Code:

(use-package emacs
  :elpaca nil
  :custom (window-combination-resize t))

(use-package windmove
  :elpaca nil
  :hook (elpaca-after-init . windmove-default-keybindings)
  :config (windmove-default-keybindings 'meta))

(use-package posframe
  :custom (posframe-mouse-banish '(0 . 5000)))

(use-package winner
  :elpaca nil
  :hook (elpaca-after-init . winner-mode)
  :general
  (irkalla/comma-lead-keydef
    "w"   '(:ignore t            :which-key "Winner Mode")
    "w p" '(delete-other-windows :which-key "Win -> Zoom-in")
    "w u" '(winner-undo          :which-key "Undo Winner")
    "w r" '(winner-redo          :which-key "Redo Winner")))

(provide 'init-frames)
;;; init-frames.el ends here
