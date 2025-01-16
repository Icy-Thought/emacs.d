;;; my-utilities.el --- Broaden The Horizon Through Various Utilities -*- lexical-binding: t; -*-

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

(require 'my-dired)
(require 'my-recent)
(require 'my-helpul)
(require 'my-vertico)
(require 'my-embark)
(require 'my-consult)
(require 'my-shackle)
(require 'my-popper)
(require 'my-eshell)
(require 'my-magit)
(require 'my-vterm)
(require 'my-email)
(require 'my-matrix)
(require 'my-telegram)
(require 'my-irc)
(require 'my-readers)
(require 'my-browser)

(provide 'my-utilities)
