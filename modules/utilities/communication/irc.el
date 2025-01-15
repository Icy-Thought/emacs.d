;;; irc.el --- Internet Relay Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package circe
  :commands (circe circe-set-display-handler)
  :config (enable-circe-color-nicks)
  :custom (circe-reduce-lurker-spam t)
  (circe-network-options
   '(("Libera Chat"
      :tls t
      :nick "Icy-Thought"
      :sasl-username "icy-thought"
      ;; :sasl-password (irkalla/read-secret-file "IRC")
      :channels ("#emacs")))))

(provide 'irkalla/irc)
