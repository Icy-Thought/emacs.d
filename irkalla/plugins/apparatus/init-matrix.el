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
  :general
  ;; Make sure that Evil does not interfere with built-in bindings:
  (:keymaps 'ement-room-mode-map
            :states '(normal motion)
            "C-s" 'ement-room-occur
            "gr" 'ement-room-sync
            "gu" 'ement-room-goto-fully-read-marker
            "<tab>" 'ement-room-goto-next
            "<backtab>" 'ement-room-goto-prev
            "q" 'quit-window
            "?" 'ement-room-transient)
  (:keymaps 'ement-room-mode-map
            :states '(normal)
            "u" (general-key "u" :state 'emacs)
            "s" (general-key "s" :state 'emacs)
            "r" (general-key "r" :state 'emacs)
            "R" (general-key "R" :state 'emacs)
            "d" 'ement-room-delete-message)
  (:keymaps 'ement-room-image-keymap
            :states '(normal motion)
            "<mouse-3>" 'ement-room-image-show)
  (:keymaps '(ement-room-list-mode-map ement-room-mode-map ement-directory-mode-map)
            :states '(normal motion)
            "RET" (general-key "RET" :state 'emacs))
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
     :password (read-secret-file "ement"))))

(provide 'init-socials)
