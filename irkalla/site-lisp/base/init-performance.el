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
  (setq-default read-process-output-max (* 24 1024 1024)))

;; Native compilation -> quicker Emacs through byte-compilations
(when (featurep 'native-compile)
  ;; :NOTE| Retain native compilation cache files in ~/.cache/emacs directory
  (let ((path (expand-file-name "var/eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path (list path)
                  native-compile-target-directory path))

  ;; :NOTE| Prevent unwanted runtime builds + reduce noise
  (setq-default comp-deferred-compilation nil
                native-comp-deferred-compilation nil
                native-comp-async-report-warnings-errors nil))

;; Collecting our ever-growing garbage...
(setq-default gc-cons-threshold most-positive-fixnum ; 2^61 bytes
              gc-cons-percentage 0.6)

(provide 'init-performance)
;;; init-performance.el ends here
