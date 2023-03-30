;;; lisp/init-management.el -*- lexical-binding: t -*-

;; Asynchronous package compilation
(use-package async
  :custom (async-bytecomp-package-mode t))

(setq native-comp-async-report-warnings-errors nil)

;; Backups: age + time of retention
(use-package emacs
  :ensure nil
  :custom
  (backup-by-copying t)
  (delete-by-moving-to-trash t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 3))

;; Defining our emacs folders:
(require 'xdg)

(setq-default
 user-emacs-config-directory (expand-file-name "emacs" (xdg-config-home))
 user-emacs-data-directory (expand-file-name "emacs" (xdg-data-home))
 user-emacs-cache-directory (expand-file-name "emacs" (xdg-cache-home)))

;; Specifying our cache & backup dir
(let ((backup-dir (expand-file-name "backup" user-emacs-cache-directory))
      (auto-save-dir (expand-file-name "auto-save" user-emacs-cache-directory)))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t)
    (mkdir auto-save-dir t))

  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        create-lockfiles nil
        backup-by-copying t))

;; Customization -> /tmp/emacs-custom-*.el
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;; (no-littering) A cleaner Emacs directory
(use-package no-littering
  :custom
  (no-littering-etc-directory (expand-file-name "config" user-emacs-data-directory))
  (no-littering-var-directory (expand-file-name "data" user-emacs-data-directory)))

(provide 'init-management)
