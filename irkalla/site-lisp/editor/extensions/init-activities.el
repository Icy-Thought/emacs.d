;;; init-activities.el --- Activities: Separation of Workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Allows splitting different tasks into several workplaces to reduce side-tracking.

;;; Code:

(use-package activities
  :elpaca (:host github :repo "alphapapa/activities.el")
  :hook (elpaca-after-init . activities-mode))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define activities-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Activities ｣──" 'mdicon "nf-md-file_tree")
            :color teal :quit-key "q")
    ("Main"
     (("l"   activities-list   "List")
      ("RET" activities-switch "Switch"))
     "Working"
     (("n"   activities-new     "Create")
      ("k"   activities-kill    "Kill")
      ("u"   activities-revert  "Revert"))
     "Breaktime"
     (("s"   activities-suspend "Suspend")
      ("r"   activities-resume  "Resume"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Control"
     (("a" activities-hydra/body "Activities")))))

(provide 'init-activities)
;;; init-activities.el ends here
