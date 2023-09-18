;;; init-centaur.el --- Centaur-Tabs: Tab & Buf. Bar -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Showcasing opened tabs for quicker navigation & recall is never a wrong thing.

;;; Code:

(use-package centaur-tabs
  :hook ((calendar-mode
          dashboard-mode
          org-agenda-mode
          vterm-mode) . centaur-tabs-local-mode)
  :general (:states 'normal :keymaps 'centaur-tabs-mode-map
             "C-<prior>" #'centaur-tabs-backward
             "C-<next>"  #'centaur-tabs-forward)
  :config (centaur-tabs-mode t)
  :custom
  (centaur-tabs-enable-key-bindings t)
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'right)
  (centaur-tabs-show-count nil)
  (centaur-tabs-left-edge-margin nil)
  (centaur-tabs-headline-match)
  :config (setq x-underline-at-descent-line t))

(provide 'init-centaur)
;;; init-centaur.el ends here
