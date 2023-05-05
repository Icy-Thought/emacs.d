;;; toolset/init-matrix.el -*- lexical-binding: t -*-

(defgroup irkalla-ement '()
  "A wonderful matrix client for Emacs!"
  :tag "Irkalla Ement"
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
  :config
  (defun irkalla/ement-auto-connect ()
    (interactive)
    (ement-connect
     :user-id "@gilganix:matrix.org"
     :password (read-secret-file "ement")
     :uri-prefix "http://localhost:8008")))

(provide 'init-matrix)
