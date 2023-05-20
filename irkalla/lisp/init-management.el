;;; lisp/init-management.el -*- lexical-binding: t -*-

;; (no-littering) A cleaner Emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config" user-emacs-data-directory))
  (setq no-littering-var-directory (expand-file-name "data" user-emacs-data-directory)))

;; Replace ~recentf~ default dirs
(use-package recentf
  :elpaca nil
  :after no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory))

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
