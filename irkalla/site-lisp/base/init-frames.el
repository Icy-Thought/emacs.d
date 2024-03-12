;;; init-frames.el --- Manipulation of Emacs Frames -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Controlling the Frames of our Emacs instance, zooming in and whatnot.

;;; Code:

(use-feature emacs
  :custom (window-combination-resize t)
  :config
  (defun irkalla/opacify-frame ()
    (let ((alpha-value
           (if (equal (frame-parameter nil 'alpha-background) 100)
               85 100)))
      (set-frame-parameter nil 'alpha-background alpha-value)
      (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))))

  (define-minor-mode irkalla/opacify-frame-mode
    "Toggle (on/off) Emacs frame transparency on demand!"
    :group 'irkalla
    :global nil
    (irkalla/opacify-frame)))

(use-feature windmove
  :hook (elpaca-after-init . windmove-default-keybindings)
  :config (windmove-default-keybindings 'meta))

(use-package posframe
  :custom (posframe-mouse-banish '(0 . 5000)))

(use-feature winner
  :hook (elpaca-after-init . winner-mode))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define window-hydra
    (:title (pretty-hydra-title "──｢ Base: Frame Management ｣──" 'mdicon "nf-md-dock_window")
            :color teal :quit-key "q")
    ("Main"
     (("o" irkalla/opacify-frame-mode "Opacify Frame" :toggle t))
     "Windows"
     (("f" delete-other-windows "Focus Window")
      ("u" winner-undo          "Restore Old Windows")
      ("r" winner-redo          "Redo Window Change"))))

  (pretty-hydra-define+ main-hydra ()
    ("Control"
     (("w" window-hydra/body "Window")))))

(provide 'init-frames)
;;; init-frames.el ends here
