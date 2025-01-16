;;; my-matrix.el --- OS & Decentralized Communication -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package ement
  :commands (ement-connect)
  :bind (:map ement-room-mode-map
              ([remap pixel-scroll-interpolate-up]   . ement-room-scroll-down-command)
              ([remap pixel-scroll-interpolate-down] . ement-room-scroll-up-mark-read))
  :config
  (when (fboundp 'visual-fill-column-mode)
    (add-hook 'ement-room-mode-hook #'visual-fill-column-mode))

  (with-eval-after-load 'cape
    (add-hook 'ement-room-read-string-setup-hook
              (lambda ()
                (setq-local completion-at-point-functions nil) ;; too much spam (members/rooms)
                (add-hook 'completion-at-point-functions #'cape-emoji nil t))))
  :custom
  (ement-notify-sound t)
  (ement-notify-notification-predicates
   '(ement-notify--event-mentions-session-user-p
     ement-notify--event-mentions-room-p))
  (ement-room-images t)
  (ement-room-message-format-spec "%S>%L %B%r%R[%t]")
  (ement-room-send-message-filter #'ement-room-send-org-filter)
  (ement-save-sessions t))


;; :NOTE| Defun later used to launch Matrix scratchpad (XMonad)

(defun irkalla/connect-to-matrix ()
  "Form a connection between Emacs and the Matrix, using Ement."
  (interactive)
  (require 'ement)
  (let* ((matrix-username "@icy-thought:matrix.org")
         (session (map-elt ement-sessions matrix-username)))
    (cond
     (session
      (message "Session already exists. Opening room list...")
      (ement-room-list))
     ((ement--read-sessions)
      (message "Connecting to a known session...")
      (call-interactively #'ement-connect))
     (t
      (message "Starting a new Ement session...")
      (ement-connect
       :user-id matrix-username
       :password (irkalla/read-secret-file "Ement")
       :uri-prefix "http://localhost:8009")))))

(provide 'my-matrix)
