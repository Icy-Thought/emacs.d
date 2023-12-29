;;; init-consult.el --- Consult: Consulting completing-read -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Better UI for our Emacs completion related tasks.

;;; Code:

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
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
  :requires (projectile))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define consult-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Consult ｣──" 'mdicon "nf-md-console")
            :color teal :quit-key "q")
    ("Main"
     (("f" consult-fd                        "Find files by NAME")
      ("r" consult-recent-file               "Recent files")
      ("p" consult-projectile-switch-project "Switch project")
      ("/" consult-ripgrep                   "Grep <- REGEXP"))
     "Action"
     (("B" consult-bookmark                  "Open named bookmark")
      ("h" consult-history                   "Insert STR from hist.")
      ("y" consult-yank-pop                  "Paste yank <- reg.")
      ("t" consult-theme                     "Switch Theme"))))

  (pretty-hydra-define editor-consult-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Consult ｣──" 'mdicon "nf-md-console")
            :color teal :quit-key "q")
    ("Language Server"
     (("m" consult-mark                      "Jump -> marker")
      ("M" consult-global-mark               "Glob. jump -> marker")
      ("o" consult-outline                   "Jump -> buffer outlines")
      ("[" consult-flymake                   "Jump -> Flymake diagnostics")
      ("]" consult-compile-error             "Jump -> compile-error in buffer"))))

  (pretty-hydra-define+ main-hydra ()
    ("Action"
     (("f" consult-hydra/body "Consult"))))

  (pretty-hydra-define+ buffer-hydra ()
    ("Consult"
     (("b" consult-buffer                    "Switch buffer")
      ("B" consult-projectile-buffer         "Switch -> project buffer")
      ("w" consult-buffer-other-window       "Buf. switch -> Split"))))

  (pretty-hydra-define+ helpful-hydra ()
    ("Action"
     (("?" consult-man                       "Consult 'MAN'-page(s)")
      ("i" consult-info                      "Consult 'MANUAL'"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Action"
     (("f" consult-hydra/body "Consult")))))

(provide 'init-consult)
;;; init-consult.el ends here
