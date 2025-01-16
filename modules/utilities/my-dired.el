;;; my-dired.el --- Interactive File/Directory Viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-mouse-drag-files t)
  (dired-kill-when-opening-new-dired-buffer t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-1-click-follows-link nil)
  (dired-movement-style 'cycle)
  (dired-listing-switches "-alFh --group-directories-first"))

;; :NOTE| Controlling various file extensions

(use-feature dired-x
  :after (dired)
  :preface
  (defun dired-external-launch (application extensions)
    "External `APPLICATION' used for launching specific file-extensions."
    (let ((pattern (rx "." extensions eos))
          (entry (list pattern application)))
      (add-to-list 'dired-guess-shell-alist-user entry)))
  :custom
  (dired-external-launch
   (if (eq system-type 'gnu/linux) "mpv" "xdg-open")
   '("avi" "flv" "mkv" "mov" "mp3" "mp4" "mpeg" "mpg" "ogg" "ogm" "wav" "wmv"))

  (dired-external-launch
   (if (eq system-type 'gnu/linux) "libreoffice" "xdg-open")
   '("doc" "docx"  "odt" "xls" "xlsx")))

;; :NOTE| Directories should have some form of highlighting

(use-package diredfl
  :after (dired)
  :hook (dired-mode . diredfl-mode)
  :custom-face (diredfl-dir-name ((t :bold t))))

(provide 'my-dired)
