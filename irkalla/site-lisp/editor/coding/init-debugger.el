;;; init-debugger.el --- Resolving The Conflicts -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Damage can easily be done and therefore we require tools that help us undo them.

;;; Code:

(use-package dape
  :requires (eglot eldoc)
  :commands (dape)
  :custom
  (dape-key-prefix "\C-x\C-a")
  (dape-buffer-window-arrangement 'right)
  (dape-cwd-fn 'projectile-project-root))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define dape-hydra
    (:title (pretty-hydra-title "──｢ Coding: Debugger ｣──" 'codicon "nf-cod-debug")
            :color teal :quit-key "q")
    ("Main"
     (("d" dape "Dape")
      ("k" dape-kill "Kill")
      ("D" dape-disconnect-quit "Disconnect")
      ("Q" dape-quit "Quit" :exit t))
     "Stepping"
     (("n" dape-next "Next")
      ("s" dape-step-in "Step In")
      ("o" dape-step-out "Step Out")
      ("c" dape-continue "Continue")
      ("p" dape-pause "Pause")
      ("r" dape-restart "Restart"))
     "Breakpoint"
     (("b" dape-breakpoint-toggle "Toggle")
      ("l" dape-breakpoint-log "Log")
      ("e" dape-breakpoint-expression "Expression")
      ("B" dape-breakpoint-remove-all "Clear"))
     "Informative"
     (("m" dape-read-memory "Read Memory")
      ("w" dape-watch-dwim "Watch DWIM")
      ("t" dape-select-thread "Select Thread")
      ("S" dape-select-stack "Select Stack")
      ("i" dape-info "Info")
      ("R" dape-repl "REPL"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Control"
     (("d" dape-hydra/body "Dape")))))

(provide 'init-debugger)
;;; init-debugger.el ends here
