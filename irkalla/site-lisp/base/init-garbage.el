;;; init-garbage.el --- Garbage Collection & Littering -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Controlling the behaviour of temporary files & memory should be handled in a sane maner, thus we introduce no-littering + gcmh.

;;; Code:

;; :NOTE| Controlling garbage collection <- quicker Emacs
(use-package gcmh
  :demand t
  :delight " Ⓖ"
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))) ; 16MB

(use-package no-littering
  :demand t
  :config
  (setopt no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
          no-littering-var-directory (expand-file-name "var/" user-emacs-directory))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory))

  (with-eval-after-load 'files
    (setopt auto-save-file-name-transforms
            `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
            backup-directory-alist
            `((".*" . ,(no-littering-expand-var-file-name "backups/"))))))

(provide 'init-garbage)
;;; init-garbage.el ends here
