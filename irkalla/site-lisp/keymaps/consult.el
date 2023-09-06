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
