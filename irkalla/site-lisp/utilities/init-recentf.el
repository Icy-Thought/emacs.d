;;; init-recentf.el --- Recentf Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Do not retain information about recently worked files for far too long, and exclude several filetypes from the list.

;;; Code:

(use-package recentf
  :elpaca nil
  :custom
  (recentf-save-file-modes #o600)
  (recentf-max-saved-items 1024)
  (recentf-auto-cleanup 600)
  (recentf-exclude '(;; Compressed files & Archives
                     "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$"
                     "\\.bz2$" "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zpaq$"
                     "\\.lz$" "\\.lrz$" "\\.lzo$" "\\.lzma$" "\\.shar$" "\\.kgb$"
                     "\\.zip$" "\\.Z$" "\\.7z$" "\\.rar$"
                     ;; TRAMP
                     "^/sudo:" "^/ssh:"
                     ;; Emacs-Everywhere
                     "/tmp/emacs-everywhere")))

(provide 'init-recentf)
;;; init-recentf.el ends here
