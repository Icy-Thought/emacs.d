;;; init-dirvish.el --- Dirvish: A Dired Frontend -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Having a different navigation UI for dired would've been useful and that we have dired to thank for!

;;; Code:

(use-package dirvish
  :hook (dired-mode . dirvish-side-follow-mode)
  :config (dirvish-override-dired-mode)
  :custom
  (dirvish-side-width 30)
  (dirvish-use-header-line t)
  (dirvish-fd-default-dir "~/")
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "~/Library/unexplored"        "Library")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group"))

;; Different syntax highlighting for directories
(use-package diredfl
  :hook ((dired-mode dirvish-directory-view-mode) . diredfl-mode)
  :custom-face (diredfl-dir-name ((t :bold t))))

(provide 'init-dirvish)
;;; init-dirvish.el ends here
