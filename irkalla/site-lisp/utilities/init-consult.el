;;; init-consult.el --- Consult: Consulting completing-read -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Better UI for our Emacs completion related tasks.

;;; Code:

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :general
  (irkalla/space-lead-keydef
    "/"          '(consult-ripgrep        :which-key "Quick RegExp grep")
    "b b"        '(consult-buffer         :which-key "Switch -> buffer")

    "p b"        '(consult-project-buffer :which-key "Switch -> project buffer")
    "p /"        '(consult-git-grep       :which-key "Grep .git repostiory")

    "f f"        '(consult-find           :which-key "File in Project")
    "f r"        '(consult-recent-file    :which-key "Recent files")

    ;; LSP-related
    "l"          '(:ignore t              :which-key "LSP & Editing")
    "l m"        '(consult-mark           :which-key "Jump -> marker")
    "l M"        '(consult-global-mark    :which-key "Glob. jump -> marker")
    "l o"        '(consult-outline        :which-key "Jump -> buffer outlines")
    "l ["        '(consult-flymake        :which-key "Jump -> Flymake diagnostics")
    "l ]"        '(consult-compile-error  :which-key "Jump -> compile-error in buffer"))

  (irkalla/comma-lead-keydef
    "c"            '(:ignore t                   :which-key "Consult")
    "c b"          '(consult-bookmark            :which-key "Open named bookmark")
    "c h"          '(consult-history             :which-key "Insert string from hist.")
    "c k"          '(consult-kmacro              :which-key "Run KBD macro")
    "c ?"          '(consult-man                 :which-key "MAN-page str search")
    "c /"          '(consult-info                :which-key "MANUALS text search")
    "c p"          '(consult-yank-pop            :which-key "Paste yanks -> cursor")
    "c t"          '(consult-theme               :which-key "Select available themes")
    "c <return>"   '(consult-mode-command        :which-key "Run command")
    "c S-<return>" '(consult-complex-command     :which-key "Evaluate CMD from hist.")
    "c w"          '(consult-buffer-other-window :which-key "Frame buffer switch"))
  :config
  (setopt register-preview-delay 0.5
          register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

    ;; Consult -> select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :custom
  (consult-narrow-key "<") ;; "C-+"
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-bookmark consult--source-file-register
                     consult--source-recent-file consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any)))

(use-package consult-projectile
  :after projectile
  :general
  (irkalla/space-lead-keydef
    "p p" '(consult-projectile-switch-project :which-key "Switch Project")))

(provide 'init-consult)
;;; init-consult.el ends here
