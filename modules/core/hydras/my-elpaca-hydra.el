;;; my-elpaca-hydra.el --- Elpaca Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'elpaca
  (pretty-hydra-define elpaca-hydra
    (:title (pretty-hydra-title "──｢ Main: Elpaca ｣──" 'pomicon "nf-pom-clean_code")
            :color teal :quit-key "q")
    ("Main"
     (("p" elpaca-manager   "Elpaca manager")
      ("r" elpaca-rebuild   "Rebuild package")
      ("i" elpaca-info      "Package info"))
     "Fetch"
     (("f" elpaca-fetch     "Specific package")
      ("e" elpaca-fetch-all "All packages"))
     "Update"
     (("m" elpaca-merge     "Specific package")
      ("a" elpaca-merge-all "All packages")))))

(provide 'my-elpaca-hydra)
