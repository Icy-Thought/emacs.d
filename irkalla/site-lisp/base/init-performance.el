;;; init-performance.el --- Performance-related Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A file containing changes which are said to have some form of performance-related benefits.

;;; Code:

;; Increase the CPU processing restrictions
(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 24 1024 1024)))

;; Native compilation -> quicker Emacs through byte-compilations
(when (featurep 'native-compile)
  ;; :NOTE| Retain native compilation cache files in ~/.cache/emacs directory
  (let ((path (expand-file-name "var/eln-cache/" user-emacs-directory)))
    (setq native-comp-eln-load-path (list path)
          native-compile-target-directory path))

  ;; :NOTE| Prevent unwanted runtime builds + reduce noise
  (setq comp-deferred-compilation nil
        native-comp-deferred-compilation nil
        native-comp-async-report-warnings-errors nil))

;; Collecting our ever-growing garbage...
(setq-default gc-cons-threshold most-positive-fixnum ; 2^61 bytes
              gc-cons-percentage 0.6)

;; :NOTE| Controlling garbage collection <- quicker Emacs
(use-package gcmh
  :demand t
  :delight " Ⓖ"
  :custom (gcmh-mode 1)
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))) ; 16MB

(use-package no-littering
  :demand t
  :config
  (setq-default no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
                no-littering-var-directory (expand-file-name "var/" user-emacs-directory))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory))

  (setq-default auto-save-file-name-transforms
                `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
                backup-directory-alist
                `((".*" . ,(no-littering-expand-var-file-name "backups/")))))

(provide 'init-performance)
;;; init-performance.el ends here
