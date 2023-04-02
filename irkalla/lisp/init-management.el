;;; lisp/init-management.el -*- lexical-binding: t -*-

;; Asynchronous package compilation
(use-package async
  :custom (async-bytecomp-package-mode t))

;; Backups: age + time of retention
(use-package emacs
  :ensure nil
  :custom
  (backup-by-copying t)
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 3))

;;; (no-littering) A cleaner Emacs directory
(use-package no-littering
  :custom
  (no-littering-etc-directory (expand-file-name "config" user-emacs-data-directory))
  (no-littering-var-directory (expand-file-name "data" user-emacs-data-directory)))

(provide 'init-management)
