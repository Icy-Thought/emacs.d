;;; init-org-roam.el --- Org-Roam: Knowledge Management -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Looking for a Zettlekasten method which works with Org-Mode? Look no further, Org-Roam is what you are looking for!

;;; Code:

(use-package org-roam
  :requires (org)
  :general
  (irkalla/comma-lead-keydef org-mode-map
    "o r"   '(:ignore t               :which-key "Org-Roam")
    "o r l" '(org-roam-buffer-toggle  :which-key "Org-Roam -> buffer")
    "o r f" '(org-roam-node-find      :which-key "Open node -> title/alias")
    "o r g" '(org-roam-graph          :which-key "Build -> show node of graph")
    "o r i" '(org-roam-node-insert    :which-key "Find node -> insert `:id` org-link")
    "o r c" '(org-roam-capture        :which-key "Open org-capture of node"))
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
  :requires (org-roam)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
