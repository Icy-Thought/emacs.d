;;; init-backup.el --- Backup-related Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Backing up files is never wrong, why not add it to the stack?

;;; Code:

;; :NOTE| Backup-related modifications
(setq-default auto-save-interval 200
              auto-save-timeout 30
              backup-by-copying t
              delete-by-moving-to-trash t
              delete-old-versions t
              kept-new-versions 7
              kept-old-versions 3
              make-backup-files t)

(provide 'init-backup)
;;; init-backup.el ends here
