;;; init-org-roam.el --- Org-Roam: Knowledge Management -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Looking for a Zettlekasten method which works with Org-Mode? Look no further, Org-Roam is what you are looking for!

;;; Code:

(use-package org-roam
  :after (org)
  :custom
  (org-roam-directory (file-truename "~/Workspace/memorandum/org-mode/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :if-new (file+head
               "%<%Y%m%d%H%M%S>-${slug}.org"
               ,(let ((options '("#+options: _:{}"
                                 "#+options: ^:{}"
                                 "#+startup: latexpreview"
                                 "#+startup: entitiespretty"
                                 "#+startup: inlineimages"
                                 "#+title: ${title}")))
                  (mapconcat 'identity options "\n")))
      :unnarrowed t)))
  (org-roam-node-display-template "${title}"))

;; :NOTE| A GUI for Org-Roam to reduce the burden of browsing
(use-package org-roam-ui
  :after (org-roam)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ org-hydra ()
    ("Org-Roam"
     (("l" org-roam-buffer-toggle "Toggle -> buffer")
      ("g" org-roam-graph         "Node <- display graph")
      ("f" org-roam-node-find     "Node <- find")
      ("i" org-roam-node-insert   "Node <- insert ':id' link")
      ("C" org-roam-capture       "Node <- Capture")))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
