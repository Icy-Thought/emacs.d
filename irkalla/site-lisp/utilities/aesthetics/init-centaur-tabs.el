;;; init-centaur-tabs.el --- Centaur-Tabs: Tab & Buf. Bar -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Showcasing opened tabs for quicker navigation & recall is never a wrong thing.

;;; Code:

(use-package centaur-tabs
  :init (if (daemonp)
            (add-hook 'server-after-make-frame-hook 'centaur-tabs-mode)
          (add-hook 'elpaca-after-init-hook 'centaur-tabs-mode))
  :bind (:map centaur-tabs-mode-map
              ("C-<prior>"   . #'centaur-tabs-backward-group)
              ("C-<next>"    . #'centaur-tabs-forward-group)
              ("M-<prior>"   . #'centaur-tabs-backward)
              ("M-<next>"    . #'centaur-tabs-forward)
              ("M-S-<prior>" . #'centaur-tabs-move-current-tab-to-left)
              ("M-S-<next>"  . #'centaur-tabs-move-current-tab-to-right))
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-enable-key-bindings t)
  (centaur-tabs-height 32)
  (centaur-tabs-left-edge-margin nil)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-show-count nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-adjust-buffer-order t)
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-group-by-projectile-project)
  (setq x-underline-at-descent-line t)

  ;; :NOTE| We do not want Centaur-Tabs everywhere, do we?
  (dolist (prefix '("*" " *" ;; all temporary buffers
                    "consult-partial-preview"
                    "Ement"
                    "magit"))
    (add-to-list 'centaur-tabs-excluded-prefixes prefix)))

(provide 'init-centaur-tabs)
;;; init-centaur-tabs.el ends here
