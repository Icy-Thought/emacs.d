;;; init-essentials.el --- Essential Emacs Changes -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Emacs modifications intending to boost our experience within this vast universe!

;;; Code:

;; :NOTE| A more helpful *Help buffer
(use-package helpful)

;; :NOTE| Spawn Emacs wherever you desire
(use-package emacs-everywhere
  :custom (emacs-everywhere-copy-command (list "cat" "%f" "|" "cb" "copy")))

(provide 'init-essentials)
;;; init-essentials.el ends here
