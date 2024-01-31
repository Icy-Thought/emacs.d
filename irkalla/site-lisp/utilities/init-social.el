;;; init-socials.el --- Socializing Inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Connecting to the world is more pleasant inside Emacs with VIM bindings.

;;; Code:

;; :NOTE| Matrix Emacs client
(use-package ement
  :commands (ement-connect)
  :hook (ement-room-mode . visual-line-mode)
  :bind (:map ement-room-mode-map
              ([remap pixel-scroll-interpolate-up]   . ement-room-scroll-down-command)
              ([remap pixel-scroll-interpolate-down] . ement-room-scroll-up-mark-read))
  :custom
  ;; :NOTE| Notifications ought to be limited to @mentions only!
  (ement-notify-notification-predicates '(ement-notify--event-mentions-session-user-p
                                          ement-notify--event-mentions-room-p))
  (ement-room-images t)
  (ement-room-message-format-spec "%S>%L %B%r%R[%t]")
  (ement-room-send-message-filter #'ement-room-send-org-filter)
  (ement-save-sessions t)
  :config
  ;; |FIXME: fix does not work when replying... somehow
  (add-hook 'ement-room-read-string-setup-hook
            (lambda ()
              (when visual-fill-column-mode (visual-fill-column-mode -1))
              (when (featurep 'cape)
                (add-hook 'completion-at-point-functions #'cape-dict nil t)
                (add-hook 'completion-at-point-functions #'cape-emoji nil t)))))

;;;###autoload
(defun irkalla/connect-to-matrix ()
  "Connect Emacs to the Matrix, unless connection exists."
  (interactive "p")
  (require 'ement)
  (let* ((matrix-username "@gilganix:matrix.org"))
    (cond
     ((map-elt ement-sessions matrix-username) (ement-room-list))
     ((ement--read-sessions)
      (call-interactively #'ement-connect)
      (message "Connecting to known Ement session..."))
     (t (ement-connect
         :user-id matrix-username
         :password (irkalla/read-secret-file "Ement")
         :uri-prefix "http://localhost:8009")))))

;; :NOTE| Code-block syntax highlighting
(use-package htmlize)

;; :NOTE| Emacs Telegram client
(use-package telega
  :elpaca nil ;; <-^ fetched from Nixpkgs
  :commands (telega)
  :hook (telega-chat-mode . visual-line-mode)
  :custom
  (telega-directory (no-littering-expand-var-file-name "telega/"))
  (telega-chat-bidi-display-reordering t)
  (telega-notifications-mode t)
  (telega-emoji-use-images nil) ;; recent libsvg issue..
  :config
  (when (featurep 'cape)
    (add-hook 'telega-chat-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'cape-dict nil t)
                (add-hook 'completion-at-point-functions #'cape-emoji nil t)))))

(provide 'init-social)
;;; init-social.el ends here
