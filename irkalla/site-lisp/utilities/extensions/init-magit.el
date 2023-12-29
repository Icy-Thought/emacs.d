;;; init-magit.el --- Magit: Version Control -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Files do change over time, thus a system to maintain the many versions of our files ought to be installed.

;;; Code:

;; :NOTE| A Magic Wand for Git
(use-package magit
  :if (executable-find "git")
  :custom
  (magit-auto-revert-mode nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package hl-todo
  :elpaca (hl-todo :version (lambda (_) "3.6.0")))

(use-package magit-todos
  :requires (magit)
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-recursive t)
  (magit-todos-depth 10)
  (magit-todos-exclude-globs '(".git/" "*.html"))
  (magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-scanner #'magit-todos--scan-with-rg)
  :config
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

;; :NOTE| Blame our Git Repository
(use-package blamer
  :if (executable-find "git")
  :hook (prog-mode . blamer-mode)
  :custom-face
  (blamer-face ((t (:background nil :height 125 :italic t))))
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-view 'overlay-right)
  (blamer-type 'visual)
  (blamer-max-commit-message-length 70)
  (blamer-force-truncate-long-line nil)
  (blamer-author-formatter " ✎ %s ")
  (blamer-commit-formatter "● \'%s\' ● "))

;; :NOTE| Symbols to Highlight Git-related Changes
(use-package git-gutter
  :if (executable-find "git")
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :custom (git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :requires (git-gutter)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; :NOTE| Finally, it's time for us to define our Hydra
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define vc-hydra
    (:title (pretty-hydra-title "──｢ Extensions: Version Control ｣──" 'mdicon "nf-md-git")
            :color teal :quit-key "q")
    ("Magit"
     (("g" magit "Magit")
      ("s" magit-stage-buffer-file   "Stage file")
      ("u" magit-unstage-buffer-file "Unstage file"))
     "Git-Gutter"
     (("m" git-gutter:mark-hunk      "Mark hunk")
      ("k" git-gutter:previous-hunk  "Previous hunk")
      ("j" git-gutter:next-hunk      "Next hunk")
      ("u" git-gutter:revert-hunk    "Revert hunk")
      ("i" git-gutter:statistic      "Stats of Buf."))))

  (pretty-hydra-define+ main-hydra ()
    ("Editor"
     (("g" vc-hydra/body "Version Control")))))

(provide 'init-magit)
;;; init-magit.el ends here
