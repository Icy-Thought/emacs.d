;;; my-nix.el --- Nix Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . (lambda ()
                         (unless (string-match-p "nixpkgs" default-directory)
                           (eglot-ensure)))))

;; :NOTE| Adding a formatting option

(when (executable-find "nixfmt")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'nix-ts-mode apheleia-mode-alist) '(nixfmt))))

(provide 'my-nix)
