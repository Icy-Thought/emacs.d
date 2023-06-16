;;; apparatus/init-social.el -*- lexical-binding: t -*-

(defgroup irkalla-social '()
  "Several chat-clients to socialize with our Emacs folks!"
  :tag "Irkalla Social"
  :group 'irkalla)

(use-package ement
  :defer t
  :hook (ement-connect . olivetti-mode)
  :custom
  (ement-room-images t)
  (ement-room-message-format-spec "%S> %W%B%r%R[%t]")
  ;; :TODO| launch side-view + limited margin to names category when in a buffer otherwise do not launch...
  ;; (ement-room-list-side-window)
  (ement-notify-notification-predicates '(ement-notify--event-mentions-session-user-p
                                          ement-notify--event-mentions-room-p)))

;;;###autoload
(defun irkalla/ement-auto-connect ()
  "Connect us to the matrix."
  (interactive)
  (ement-connect
   :user-id "@gilganix:matrix.org"
   :password (irkalla/read-secret-file "ement")
   :uri-prefix "http://127.0.0.1:8009"))

(provide 'init-social)
