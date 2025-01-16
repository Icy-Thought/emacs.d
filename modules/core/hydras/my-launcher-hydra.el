;;; my-launcher-hydra.el --- Launcher Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/


(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define launcher-hydra
    (:title (pretty-hydra-title "──｢ Main: Launcher(s) ｣──" 'codicon "nf-cod-rocket")
            :color teal :quit-key "q")
    ("Browser"
     (("w" (eww-browse-url "https://en.wikipedia.org") "Wikipedia"))
     "Application"
     (("RET" dashboard-refresh-buffer        "Dashboard")
      ("n"   irkalla/newsticker-start-newTab "RSS"))
     "Terminal"
     (("t" vterm                      "Terminal")
      ("l" project-eshell             "Project Eshell")
      ("n" nix-shell                  "Nix Eshell")))))

(provide 'my-launcher-hydra)
