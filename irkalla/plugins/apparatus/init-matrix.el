;;; apparatus/init-matrix.el -*- lexical-binding: t -*-

(defgroup irkalla-socials '()
  "Several chat-clients to socialize with our Emacs folks!"
  :tag "Irkalla Socials"
  :group 'irkalla)

(defun read-secret-file (filename)
  "Read the contents of a file in /run/secrets and return the output as a string."
  (with-temp-buffer
    (insert-file-contents (concat "/run/agenix/" filename))
    (string-trim-right (buffer-string))))

(use-package ement
  :custom
  (ement-room-images t)
  ;; (ement-room-list-side-window) ;; :TODO| launch side-view + limited margin to names category when in a buffer otherwise do not launch...
  (ement-notify-notification-predicates '(ement-notify--event-mentions-session-user-p
                                          ement-notify--event-mentions-room-p))
  :config
  (defun irkalla/ement-auto-connect ()
    (interactive)
    (ement-connect
     :uri-prefix "http://localhost:8009"
     :user-id "@gilganix:matrix.org"
     :password (read-secret-file "ement")))

  ;; Make sure that Evil does not interfere with built-in bindings:
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ement-room-list-mode 'emacs)
    (evil-set-initial-state 'ement-room-mode 'emacs)
    (evil-set-initial-state 'ement-taxy-mode 'emacs)))

(provide 'init-socials)
