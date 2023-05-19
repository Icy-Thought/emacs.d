;;; lisp/init-management.el -*- lexical-binding: t -*-

;; (no-littering) A cleaner Emacs directory
(use-package no-littering
  :custom
  (no-littering-etc-directory (expand-file-name "config" user-emacs-data-directory))
  (no-littering-var-directory (expand-file-name "data" user-emacs-data-directory)))

;; Asynchronous package compilation
(use-package async
  :custom (async-bytecomp-package-mode t))

;; Appropriate garbage collection
(use-package gcmh
  :init (gcmh-mode 1))

;; Backups: age + time of retention
(use-package emacs
  :elpaca nil
  :custom
  (backup-by-copying t)
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 3))

(provide 'init-management)
