;;; my-recent.el --- Tracking Recent Activity -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature recentf
  :hook (elpaca-after-init . recentf-mode)
  :custom
  (recentf-case-fold-search t)
  (recentf-max-saved-items 450)
  (recentf-exclude
   `(,(rx bos "/tmp/")
     ,(rx bos "/nix/store")

     ;; :NOTE| Compressed files & Archives
     ,(rx "."
          (| "tar" "tbz2" "tbz" "tgz"
             "bz2" "bz" "gz" "gzip" "xz" "zpaq"
             "lz" "lrz" "lzo" "lzma" "shar" "kgb"
             "zip" "Z" "7z" "rar")
          eos)

     ;; :NOTE| TRAMP
     ,(rx bos "/sudo:")
     ,(rx bos "/ssh:"))))

(provide 'my-recent)
