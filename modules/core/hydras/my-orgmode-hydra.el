;;; my-orgmode-hydra.el --- Org-Mode Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define org-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Org-Mode ｣──" 'sucicon "nf-custom-orgmode")
            :color teal :quit-key "q")
    ("Buffer"
     (("c" org-capture         "Capture")
      ("e" org-export-dispatch "Export")
      ("t" org-babel-tangle    "Tangle"))
     "Org-Roam"
     (("l" org-roam-buffer-toggle "Toggle -> buffer" :toggle t)
      ("g" org-roam-graph         "Node <- display graph")
      ("f" org-roam-node-find     "Node <- find")
      ("i" org-roam-node-insert   "Node <- insert ':id' link")
      ("C" org-roam-capture       "Node <- Capture"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Markup"
     (("o" (if (eq major-mode 'org-mode)
               (org-hydra/body)
             (message "You are not in an Org buffer.")) "Org-Mode")))))

(provide 'my-orgmode-hydra)
