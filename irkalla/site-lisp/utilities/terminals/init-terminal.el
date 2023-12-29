;;; init-terminal.el --- Terminal-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Terminals are essential and sometimes we want Emacs to behave in a certain way, hence the config!

;;; Code:

(use-package tramp
  :elpaca nil
  :custom
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-cache nil))

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ launcher-hydra ()
    ("Terminal"
     (("p" eat-project    "EAT (Project)")
      ("e" project-eshell "Eshell -> Project")
      ("n" nix-shell      "Eshell -> Nix")))))

(irkalla/enable-modules (eshell eat))

(provide 'init-terminal)
;;; init-terminal.el ends here
