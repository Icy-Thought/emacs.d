;;; editor/init-org-modern.el -*- lexical-binding: t -*-

(defgroup irkalla-org-modern '()
  "a modernized user interface for our beloved org-mode"
  :tag "Irkalla Org-Modern"
  :group 'irkalla)

(use-package org-modern
  :after org
  :init
  (global-org-modern-mode))

(provide 'init-org-modern)
