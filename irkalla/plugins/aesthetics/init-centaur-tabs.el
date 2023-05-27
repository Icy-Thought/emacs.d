;;; ui/init-centaur-tabs.el -*- lexical-binding: t -*-

(defgroup irkalla-centaur-tabs '()
  "a rather modern looking tabs plugin"
  :tag "Irkalla Centaur-Tabs"
  :group 'irkalla)

(use-package centaur-tabs
  :hook (elpaca-after-init . centaur-tabs-mode)
  :general (centaur-tabs-mode-map
            "C-<prior>" #'centaur-tabs-backward
            "C-<next>"  #'centaur-tabs-forward)
  :init (setq centaur-tabs-enable-key-bindings t)
  :custom
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'right)
  (centaur-tabs-show-count nil)
  (centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  :config (setq x-underline-at-descent-line t))

(provide 'init-centaur-tabs)
