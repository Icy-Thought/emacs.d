;;; lisp/init-management.el -*- lexical-binding: t -*-

;; Defining our emacs folders:
(require 'xdg)

(setq-default
 user-emacs-config-directory (concat (xdg-config-home) "/emacs")
 user-emacs-data-directory (concat (xdg-data-home) "/emacs")
 user-emacs-cache-directory (concat (xdg-cache-home) "/emacs"))

;; Specifying our cache & backup dir
(let ((backup-dir (concat user-emacs-cache-directory "/backup"))
      (auto-save-dir (concat user-emacs-cache-directory "/auto-save")))
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
  :init
  (setq-default
   no-littering-etc-directory (concat user-emacs-data-directory "etc")
   no-littering-var-directory (concat user-emacs-data-directory "var")))

(provide 'init-management)
