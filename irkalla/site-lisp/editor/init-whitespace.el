;;; init-whitespace.el --- Whitespace: Empty Spaces Recognition -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Managing the looks & behaviour of our ever-growing whitespaces is important.

;;; Code:

(use-package whitespace
  :elpaca nil
  ;; :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  (whitespace-style
   '(face spaces tabs newline trailing space-mark tab-mark newline-mark))
  (whitespace-display-mappings
   '(;; space -> · else .
     (space-mark 32 [183] [46])
     ;; new line -> ¬ else $
     (newline-mark ?\n [172 ?\n] [36 ?\n])
     ;; carriage return (Windows) -> ¶ else #
     (newline-mark ?\r [182] [35])
     ;; tabs -> » else >
     (tab-mark ?\t [187 ?\t] [62 ?\t]))))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
