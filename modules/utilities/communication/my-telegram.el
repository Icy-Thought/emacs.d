;;; my-telegram.el --- A client for Telegram -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature telega
  :commands (telega)
  :config
  (advice-add 'telega-chatbuf-recenter-1
              :around (lambda (orig-fun &rest args) (recenter -2)))

  (when (fboundp 'visual-fill-column-mode)
    (add-hook 'telega-chat-mode-hook #'visual-fill-column-mode))

  ;; :NOTE| Enable dictionary + emoji suggestions in compose area
  (with-eval-after-load 'cape
    (add-hook 'telega-chat-mode-hook
              (lambda () (add-hook 'completion-at-point-functions #'cape-emoji nil t))))
  :custom
  (telega-emoji-use-images nil) ;; :WARN| libsvg issue -> odd symbols
  (telega-directory (no-littering-expand-var-file-name "telega/"))
  (telega-chat-bidi-display-reordering t)
  (telega-notifications-mode t))

(provide 'my-telegram)
