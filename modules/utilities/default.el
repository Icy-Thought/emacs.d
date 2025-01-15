;;; default.el --- Broaden The Horizon Through Various Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Bring forth Emacs client for editing purposes

(use-package emacs-everywhere
  :commands (emacs-everywhere)
  :custom (emacs-everywhere-copy-command '("sh" "-c" "cat %f | cb copy")))

;; :NOTE| Trace startup performance hiccups

(use-package esup
  :commands (esup)
  :custom (esup-depth 0))

;; :NOTE| Transparent Remote Access (multi) Protocol

(use-feature tramp
  :defer t
  :config
  (setopt remote-file-name-inhibit-cache nil)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  :custom
  (tramp-verbose 0)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil))

;; :NOTE| Syntax highlighting for various uses

(use-package htmlize :defer t)

;; :NOTE| Time to require the modules

(require 'irkalla/dired)
(require 'irkalla/recent)
(require 'irkalla/help)
(require 'irkalla/vertico)
(require 'irkalla/embark)
(require 'irkalla/consult)
(require 'irkalla/shackle)
(require 'irkalla/popper)
(require 'irkalla/eshell)
(require 'irkalla/vterm)
(require 'irkalla/mu4e)
(require 'irkalla/matrix)
(require 'irkalla/telegra)
(require 'irkalla/irc)
(require 'irkalla/readers)
(require 'irkalla/browser)

;; :WARN| Load Hydra last to ensure commands have been loaded.
(require 'irkalla/hydra)

(provide 'irkalla/utilities)
