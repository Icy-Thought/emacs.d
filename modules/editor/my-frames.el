;;; my-frames.el --- Rules For Emacs Frames -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Isolated Emacs frames

(use-package beframe
  :bind ([remap list-buffers] . beframe-switch-buffer)
  :hook (elpaca-after-init . beframe-mode)
  :config
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-command-categories '(kill-buffer . buffer))
    (add-to-list 'marginalia-command-categories '(beframe-switch-buffer . buffer)))

  ;; :NOTE| Consult menu for Beframe buffers
  (with-eval-after-load 'consult
    (defface beframe-buffer '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun my-beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my-beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (setopt consult-buffer-sources '(beframe-consult-source)))
  :custom (beframe-rename-function nil))

;; :NOTE| Emacs session management

(use-feature desktop
  :hook (elpaca-after-init . desktop-save-mode)
  :custom
  (desktop-base-file-name "last-session")
  (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  (desktop-restore-eager 25)
  (desktop-file-checksum t)
  (desktop-save-buffer t)
  (desktop-save t))

;; :NOTE| Management of random ideas

(use-feature project
  :custom
  (project-vc-extra-root-markers
   '(".repo" ".dir-locals.el" ".project" ".project.el" ".projectile.el"
     "Makefile" "CMakeLists.txt" "xmake.lua"
     "Cargo.toml"
     "Dockerfile" "autogen.sh"
     "package.json" "requirements.txt")))

(provide 'my-frames)
