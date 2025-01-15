;;; default.el --- Buffers Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(pretty-hydra-define buffer-hydra
  (:title (pretty-hydra-title "──｢ Main: Buffer(s) ｣──" 'octicon "nf-oct-repo_template")
          :color teal :quit-key "q")
  ("Buffer"
   (("s" scratch-buffer   "Scratch")
    ("j" next-buffer      "Next")
    ("k" previous-buffer  "Previous"))))

(provide 'irkalla/hydra-buffers)
