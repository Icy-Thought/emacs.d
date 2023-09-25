;;; init-frames.el --- Manipulation of Emacs Frames -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Controlling the Frames of our Emacs instance, zooming in and whatnot.

;;; Code:

(use-package emacs
  :elpaca nil
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (cursor-in-non-selected-windows nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (echo-keystrokes 0.02)
  (enable-recursive-minibuffers t)
  (help-window-select t)
  (read-buffer-completion-ignore-case t)
  (read-process-output-max (* 64 1024));
  (ring-bell-function 'ignore)
  (debug-on-error init-file-debug)
  (jka-compr-verbose init-file-debug))

(use-package perspective
  :hook (elpaca-after-init . persp-mode)
  :custom (persp-mode-prefix-key (kbd "C-c M-p")))

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
