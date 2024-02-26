;;; init-terminal.el --- Terminal-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Terminals are essential and sometimes we want Emacs to behave in a certain way, hence the config!

;;; Code:

(use-feature tramp
  :config
  (setopt remote-file-name-inhibit-cache nil)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  :custom
  (tramp-verbose 0)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ launcher-hydra ()
    ("Terminal"
     (("e" eat            "EAT")
      ("p" eat-project    "EAT -> Project")
      ("l" project-eshell "Eshell -> Project")
      ("n" nix-shell      "Eshell -> Nix")))))

;; :NOTE| Import the custom modules
(require 'init-eshell)
(require 'init-eat)

(provide 'init-terminal)
;;; init-terminal.el ends here
