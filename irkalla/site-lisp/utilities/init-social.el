;;; init-socials.el --- Socializing Inside Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Connecting to the world is more pleasant inside Emacs with VIM bindings.

;;; Code:

;; :NOTE| Matrix Emacs client
(use-package ement
  :hook (ement-room-mode . olivetti-mode)
  :custom
  ;; :NOTE| Notifications ought to be limited to @mentions only!
  (ement-notify-notification-predicates '(ement-notify--event-mentions-session-user-p
                                          ement-notify--event-mentions-room-p))
  (ement-room-images t)
  (ement-room-message-format-spec "%S>%L %B%r%R[%t]")
  (ement-room-send-message-filter #'ement-room-send-org-filter)
  (ement-save-sessions t))

(defun irkalla/connect-to-matrix ()
  "Connect Emacs to the Matrix, unless connection exists."
  (interactive)
  (require 'ement)
  (let* ((matrix-username "@gilganix:matrix.org"))
    (cond
      ((map-elt ement-sessions matrix-username)
       (ement-room-list))
      ((ement--read-sessions)
       (call-interactively #'ement-connect))
      (t (ement-connect
          :user-id matrix-username
          :password (irkalla/read-secret-file "ement")
          :uri-prefix "http://localhost:8009")))))

;; :NOTE| Code-block syntax highlighting
(use-package htmlize
  :defer t)

;; :NOTE| Emacs Telegram client
(use-package telega
  :elpaca nil ;; <-^ fetched from Nixpkgs
  :defer 2)

(provide 'init-social)
;;; init-social.el ends here
