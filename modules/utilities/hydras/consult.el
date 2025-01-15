;;; default.el --- Consult Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define consult-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Consult ｣──" 'mdicon "nf-md-console")
            :color teal :quit-key "q")
    ("Main"
     (("f" consult-fd                        "Find files by NAME")
      ("r" consult-recent-file               "Recent files")
      ("s" consult-project-extra-find        "Switch project")
      ("/" consult-ripgrep                   "Grep <- REGEXP"))
     "Action"
     (("B" consult-bookmark                  "Open named bookmark")
      ("h" consult-history                   "Insert STR from hist.")
      ("p" consult-yank-pop                  "Paste yank <- reg.")
      ("t" consult-theme                     "Switch Theme"))))

  (pretty-hydra-define+ main-hydra ()
    ("Action"
     (("f" consult-hydra/body "Consult"))))

  (pretty-hydra-define editor-consult-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Consult ｣──" 'mdicon "nf-md-console")
            :color teal :quit-key "q")
    ("Jump To"
     (("m" consult-mark                      "Marker")
      ("M" consult-global-mark               "Glob. Marker")
      ("o" consult-outline                   "Buffer Outlines")
      ("f" consult-flymake                   "Flymake Diagnostics")
      ("e" consult-compile-error             "Buffer Compile Errors"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Control"
     (("f" editor-consult-hydra/body "Consult"))))

  (pretty-hydra-define+ buffer-hydra ()
    ("Consult"
     (("b" consult-buffer                    "Switch Buffer")
      ("B" consult-project-buffer            "Project Buf. Switch")
      ("w" consult-buffer-other-window       "Split Buf. Switch"))))

  (pretty-hydra-define+ helpful-hydra ()
    ("Action"
     (("?" consult-man                       "Consult MAN-page(s)")
      ("i" consult-info                      "Consult MANUAL")))))

(provide 'irkalla/hydra-consult)
