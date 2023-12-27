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

;; :NOTE| Finally, it's time for us to define our Hydra
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define consult-hydra
    (:title (pretty-hydra-title "──｢ Extensions: Consult ｣──" 'mdicon "nf-md-console")
            :color teal :quit-key "q")
    ("Action(s)"
     (("j" consult-line                      "Jump -> Searched Line")
      ("r" consult-recent-file               "Recent files")
      ("b" consult-buffer                    "Switch buffer")
      ("B" consult-projectile-buffer         "Switch -> project buffer")
      ("p" consult-projectile-switch-project "Switch Project")
      ("/" consult-ripgrep                   "REGEXP grep"))
     "Language Server"
     (("m" consult-mark                      "Jump -> marker")
      ("M" consult-global-mark               "Glob. jump -> marker")
      ("o" consult-outline                   "Jump -> buffer outlines")
      ("[" consult-flymake                   "Jump -> Flymake diagnostics")
      ("]" consult-compile-error             "Jump -> compile-error in buffer"))
     "Emacs"
     (("B" consult-bookmark                  "Open named bookmark")
      ("h" consult-history                   "Insert string from hist.")
      ("k" consult-kmacro                    "Run KBD macro")
      ("?" consult-man                       "MAN-page str search")
      ("i" consult-info                      "MANUALS text search")
      ("y" consult-yank-pop                  "Paste yanks -> cursor")
      ("t" consult-theme                     "Select available themes")
      ("c" consult-mode-command              "Run command")
      ("C" consult-complex-command           "Evaluate CMD from hist.")
      ("w" consult-buffer-other-window       "Frame buffer switch")))))

(provide 'init-consult)
;;; init-consult.el ends here
