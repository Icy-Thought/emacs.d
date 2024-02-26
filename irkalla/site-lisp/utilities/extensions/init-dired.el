;;; init-dired.el --- Dired-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Dired is not perfect, but it's wonderful! Thus we ought to expand its horizons!!

;;; Code:

(use-feature dired
  :config
  (setopt mouse-drag-and-drop-region-cross-program t
          mouse-1-click-follows-link nil)
  :custom (dired-mouse-drag-files t))

(use-feature dired-x
  :after (dired)
  :preface
  (defun dired-external-launch (application extensions)
    "External `APPLICATION' used for launching specific file-extensions."
    (let ((pattern (concat "\\." (regexp-opt extensions t) "$"))
          (entry (list pattern application)))
      (add-to-list 'dired-guess-shell-alist-user entry)))
  :custom
  (dired-external-launch
   (if (eq system-type 'gnu/linux) "mpv" "xdg-open")
   '("avi" "flv" "mkv" "mov" "mp3" "mp4" "mpeg" "mpg" "ogg" "ogm" "wav" "wmv"))

  (dired-external-launch
   (if (eq system-type 'gnu/linux) "libreoffice" "xdg-open") 
   '("doc" "docx"  "odt" "xls" "xlsx")))

;; Different syntax highlighting for directories
(use-package diredfl
  :hook ((dired-mode dirvish-directory-view-mode) . diredfl-mode)
  :custom-face (diredfl-dir-name ((t :bold t))))

;; Alternative frontend for dired
(use-package dirvish
  :commands (dirivish-side)
  :hook ((dired-mode . dirvish-override-dired-mode))
  :bind (("C-c f" . dirvish-side)
         :map dirvish-mode-map
         ("a"   . dirvish-quick-access)
         ("f"   . dirvish-file-info-menu)
         ("y"   . dirvish-yank-menu)
         ("N"   . dirvish-narrow)
         ("^"   . dirvish-history-last)
         ("h"   . dirvish-history-jump)
         ("s"   . dirvish-quicksort)
         ("v"   . dirvish-vc-menu)
         ("TAB" . dirvish-subtree-toggle)
         ("M-f" . dirvish-history-go-forward)
         ("M-b" . dirvish-history-go-backward)
         ("M-l" . dirvish-ls-switches-menu)
         ("M-m" . dirvish-mark-menu)
         ("M-t" . dirvish-layout-toggle)
         ("M-s" . dirvish-setup-menu)
         ("M-e" . dirvish-emerge-menu)
         ("M-j" . dirvish-fd-jump)
         ("<mouse-1>" . dirvish-subtree-toggle-or-open)
         ("<mouse-2>" . dired-mouse-find-file-other-window)
         ("<mouse-3>" . dired-mouse-find-file))
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  :custom
  (dirvish-side-width 30)
  (dirvish-use-header-line t)
  (dirvish-fd-default-dir "~/")
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("l" "~/Library/unexplored"        "Library")
     ("t" "~/.local/share/Trash/files/" "Rubbish Bin")))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))

(provide 'init-dired)
;;; init-dired.el ends here
