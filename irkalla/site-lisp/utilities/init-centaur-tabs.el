;;; init-centaur-tabs.el --- Centaur-Tabs: Tab & Buf. Bar -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Showcasing opened tabs for quicker navigation & recall is never a wrong thing.

;;; Code:

(use-package centaur-tabs
  :hook (((calendar-mode org-agenda-mode) . centaur-tabs-local-mode)
         ((dashboard-mode vterm-mode) . centaur-tabs-local-mode) 
         ((Ement-Room Ement-Room-List) . centaur-tabs-local-mode)) 
  :general (:states 'normal :keymaps 'centaur-tabs-mode-map
                    "C-<prior>" #'centaur-tabs-backward-group
                    "C-<next>"  #'centaur-tabs-forward-group
                    "M-<prior>" #'centaur-tabs-backward
                    "M-<next>"  #'centaur-tabs-forward
                    "M-S-<prior>" #'centaur-tabs-move-current-tab-to-left
                    "M-S-<next>"  #'centaur-tabs-move-current-tab-to-right)
  :config
  (centaur-tabs-mode t)
  (setq x-underline-at-descent-line t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-headline-match)
  (centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
  (centaur-tabs-enable-key-bindings t)
  (centaur-tabs-height 32)
  (centaur-tabs-left-edge-margin nil)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-show-count nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-show-new-tab-button t))

(provide 'init-centaur-tabs)
;;; init-centaur-tabs.el ends here
