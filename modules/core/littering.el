;;; littering.el --- Teach Emacs To Litter Properly -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package no-littering
  :demand t
  :config
  (setq no-littering-etc-directory (expand-file-name "etc" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var" user-emacs-directory))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-etc-directory)))

  (with-eval-after-load 'files
    (setopt auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
            backup-directory-alist
            `((".*" . ,(no-littering-expand-var-file-name "backups/"))))))

(provide 'irkalla/littering)
