;;; default.el --- Magit Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define vc-hydra
    (:title (pretty-hydra-title "──｢ Editor: Version Control ｣──" 'mdicon "nf-md-git")
            :color teal :quit-key "q")
    ("Magit"
     (("g" magit                     "Open Magit")
      ("s" magit-stage-buffer-file   "Stage file")
      ("u" magit-unstage-buffer-file "Unstage file")
      ("b" magit-branch-checkout     "Checkout Branch"))
     "Git-Gutter"
     (("m" git-gutter:mark-hunk             "Mark hunk")
      ("k" git-gutter:previous-hunk         "Previous hunk")
      ("j" git-gutter:next-hunk             "Next hunk")
      ("u" git-gutter:revert-hunk           "Revert hunk"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Control"
     (("g" vc-hydra/body "Version Control")))))

(provide 'irkalla/hydra-magit)
