;;; my-helpul-hydra.el --- Help Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define helpful-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Helpful ｣──" 'mdicon "nf-md-help_network")
            :color teal :quit-key "q")
    ("Describe"
     (("k" helpful-key      "Key(s)")
      ("f" helpful-function "Function(s)")
      ("F" helpful-callable "Interactive function(s)")
      ("v" helpful-variable "Variable(s)")
      ("c" helpful-command  "Command(s)"))
     "Action"
     (("p" helpful-at-point "SYMB at point")
      ("d" devdocs-lookup   "Lookup DevDocs")))))

(provide 'my-helpul-hydra)
