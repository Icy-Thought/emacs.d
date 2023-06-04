;;; apparatus/init-consult.el -*- lexical-binding: t -*-

(defgroup irkalla-consult '()
  "Search & navigation commands."
  :tag "Irkalla Consult"
  :group 'irkalla)

(use-package consult
  :general
  (irkalla/space-lead-keydef
    "/"          '(consult-ripgrep        :which-key "Quick Regex grep from current dir")
    "b b"        '(consult-buffer         :which-key "Switch (+visualize) to buffer")

    "p"          '(:ignore t              :which-key "Project")
    "p b"        '(consult-project-buffer :which-key "Switch (+visualize) to project buffer")
    "p /"        '(consult-git-grep       :which-key "Grep current .git repostiory")

    "f"          '(:ignore t              :which-key "Files")
    "f f"        '(consult-find           :which-key "Find file based on its given name")
    "f r"        '(consult-recent-file    :which-key "Open file based on last time it was edited")

    ;; LSP-related
    "l"          '(:ignore t              :which-key "LSP & Editing")
    "l m"        '(consult-mark           :which-key "Jump to marker in MARKER-list")
    "l M"        '(consult-global-mark    :which-key "Global jump to marker in MARKER-list")
    "l o"        '(consult-outline        :which-key "Jump to buffer outlines (headings)")
    "l ["        '(consult-flymake        :which-key "Jump to Flymake diagnostics")
    "l ]"        '(consult-compile-error  :which-key "Jump to compile-error in buffer"))

  (irkalla/comma-lead-keydef
    "c"            '(:ignore t                   :which-key "Consult")
    "c b"          '(consult-bookmark            :which-key "Open/Create named bookmark")
    "c h"          '(consult-history             :which-key "Insert string from history of buffer")
    "c k"          '(consult-kmacro              :which-key "Run a chosen KBD macro")
    "c ?"          '(consult-man                 :which-key "String search for MAN-page")
    "c /"          '(consult-info                :which-key "Full-text search through MANUALS")
    "c p"          '(consult-yank-pop            :which-key "Paste past yanks at cursor")
    "c <return>"   '(consult-mode-command        :which-key "Run a CMD from ANY mode")
    "c S-<return>" '(consult-complex-command     :which-key "Select & Evaluate CMD from history")
    "c w"          '(consult-buffer-other-window :which-key "Buffer switch (+visualize) in frame"))

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Consult -> select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<") ;; "C-+"

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(provide 'init-consult)
