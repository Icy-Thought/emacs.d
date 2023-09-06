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
  :general
  (irkalla/comma-lead-keydef
    "f"   '(:ignore t    :which-key "Dirvish")
    "f f" '(dirvish-side :which-key "Dirvish Side-View")
    "f /" '(dirvish-fd   :which-key "Run fd in dir"))

  (irkalla/comma-lead-keydef
    :keymaps 'dirvish-mode-map
    "a"   '(dirvish-quick-access        :which-key "Frequently used dirs")
    "f"   '(dirvish-file-info-menu      :which-key "File information")
    "y"   '(dirvish-yank-menu           :which-key "Yank marked files")
    "N"   '(dirvish-narrow              :which-key "Live narrowing")
    "^"   '(dirvish-history-last        :which-key "Goto recent buffer")
    "h"   '(dirvish-history-jump        :which-key "Goto recent dirs") ; remapped `describe-mode'
    "s"   '(dirvish-quicksort           :which-key "Sort buffers")     ; remapped `dired-sort-toggle-or-edit'
    "v"   '(dirvish-vc-menu             :which-key "Version control")  ; remapped `dired-view-file'
    "TAB" '(dirvish-subtree-toggle      :which-key "Dir -> sub-tree")
    "M-f" '(dirvish-history-go-forward  :which-key "History -> forward")
    "M-b" '(dirvish-history-go-backward :which-key "History -> backward")
    "M-l" '(dirvish-ls-switches-menu    :which-key "Setup listing switches")
    "M-m" '(dirvish-mark-menu           :which-key "Manage Marks")
    "M-t" '(dirvish-layout-toggle       :which-key "Toggle Fullscreen")
    "M-s" '(dirvish-setup-menu          :which-key "User Interface Setup")
    "M-e" '(dirvish-emerge-menu         :which-key "Manage Emerged Groups")
    "M-j" '(dirvish-fd-jump             :which-key "Setup fd-find Switches"))
  :config (dirvish-override-dired-mode)
  :custom
  (dirvish-side-width 30)
  (dirvish-use-header-line t)
  (dirvish-fd-default-dir "~/")
  (dirvish-quick-access-entries '(("h" "~/"                          "Home")
                                  ("d" "~/Downloads/"                "Downloads")
                                  ("m" "~/Library/unexplored"        "Library")
                                  ("t" "~/.local/share/Trash/files/" "Trash")))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))

;; Different syntax highlighting for directories
(use-package diredfl
  :hook ((dired-mode dirvish-directory-view-mode) . diredfl-mode)
  :custom-face (diredfl-dir-name ((t :bold t))))

(provide 'init-dirvish)
;;; init-dirvish.el ends here
