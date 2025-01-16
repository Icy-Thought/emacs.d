;;; my-embark-hydra.el --- Embark Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define embark-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Embark ｣──" 'mdicon "nf-md-lightbulb_on_outline")
            :color teal :quit-key "q")
    ("Action(s)"
     (("a" embark-act      "Prompt -> perform")
      ("d" embark-dwim     "Run default on buffer"))
     "Documentation"
     (("h" embark-bindings "Explore Emacs bindings"))))

  (pretty-hydra-define+ main-hydra ()
    ("Action"
     (("e" embark-hydra/body "Embark")))))

(provide 'my-embark-hydra)
